import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import { mkdtemp, mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const binary = process.env.BLASTER_RPC_BINARY ?? join(root, 'api/.lake/build/bin/blaster-rpc-api');

class Client {
  buffer = Buffer.alloc(0);
  messages = [];
  waiters = [];
  sequence = 0;
  stderr = '';

  constructor() {
    this.child = spawn(binary, [], { cwd: join(root, 'api') });
    this.exited = once(this.child, 'exit');
    this.child.stderr.on('data', chunk => { this.stderr += chunk; });
    this.child.stdout.on('data', chunk => {
      this.buffer = Buffer.concat([this.buffer, chunk]);
      while (true) {
        const headerEnd = this.buffer.indexOf('\r\n\r\n');
        if (headerEnd < 0) return;
        const header = this.buffer.subarray(0, headerEnd).toString();
        const match = /^Content-Length: (\d+)/im.exec(header);
        assert.ok(match, header);
        const end = headerEnd + 4 + Number(match[1]);
        if (this.buffer.length < end) return;
        const message = JSON.parse(this.buffer.subarray(headerEnd + 4, end).toString());
        this.buffer = this.buffer.subarray(end);
        const index = this.waiters.findIndex(waiter => waiter.predicate(message));
        if (index < 0) this.messages.push(message);
        else this.waiters.splice(index, 1)[0].resolve(message);
      }
    });
  }

  send(method, params, id) {
    const body = JSON.stringify({ jsonrpc: '2.0', method, params, ...(id === undefined ? {} : { id }) });
    this.child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
  }

  wait(predicate) {
    const index = this.messages.findIndex(predicate);
    if (index >= 0) return Promise.resolve(this.messages.splice(index, 1)[0]);
    return new Promise((resolveMessage, reject) => {
      const waiter = { predicate, resolve: message => { clearTimeout(timer); resolveMessage(message); } };
      const timer = setTimeout(() => {
        this.waiters.splice(this.waiters.indexOf(waiter), 1);
        reject(new Error(`RPC timeout. stderr: ${this.stderr}`));
      }, 120000);
      this.waiters.push(waiter);
    });
  }

  request(method, params) {
    const id = ++this.sequence;
    this.send(method, params, id);
    return this.wait(message => message.id === id);
  }

  async eventsUntilFinished(count) {
    const events = [];
    let finished = 0;
    while (finished < count) {
      const { params: event } = await this.wait(message => message.method === 'testEvent');
      events.push(event);
      assert.equal(typeof event.payload.workspaceId, 'string');
      assert.equal(typeof event.payload.testId, 'string');
      if (event.type === 'test-error' || (event.type === 'test-update' && event.payload.status)) finished++;
    }
    return events;
  }

  async close() {
    if (this.child.exitCode !== null) return;
    this.send('exit', {});
    const timeout = setTimeout(() => this.child.kill('SIGKILL'), 3000);
    const [code, signal] = await this.exited;
    clearTimeout(timeout);
    assert.equal(signal, null);
    assert.equal(code, 0);
  }
}

function testsIn(nodes) {
  return Object.values(nodes).flatMap(node => node.type === 'file' ? Object.values(node.tests) : testsIn(node.nodes));
}

test('Blaster RPC contract', { timeout: 240000 }, async context => {
  const temporary = await mkdtemp(join(tmpdir(), 'blaster-rpc-'));
  const project = join(temporary, 'project');
  const nested = join(project, 'nested');
  const empty = join(temporary, 'empty');
  await mkdir(nested, { recursive: true });
  await mkdir(empty);
  await writeFile(join(project, 'lakefile.toml'), 'name = "rpc-test"\nversion = "0.1.0"\n');
  await writeFile(join(project, 'lean-toolchain'), await readFile(join(root, 'api/lean-toolchain')));
  await writeFile(join(nested, 'Basic.lean'), 'theorem first : True := by trivial\ntheorem second : 1 = 1 := by rfl\n');
  await writeFile(join(nested, 'Broken.lean'), 'theorem tooling_error : True := by missing_tactic\n');
  const client = new Client();
  context.after(async () => { await client.close(); await rm(temporary, { recursive: true, force: true }); });
  await client.wait(message => message.method === 'serverReady');
  const workspaces = [{ id: 'outer', path: project }, { id: 'inner', path: nested }, { id: 'empty', path: empty }];
  let tree;

  await context.test('discovery shape, empty roots, stable IDs and legacy removal', async () => {
    assert.deepEqual((await client.request('fetch', { workspaces: [] })).result, { testTree: { workspaces: {} } });
    const response = await client.request('fetch', { workspaces });
    assert.equal(response.error, undefined, JSON.stringify(response));
    tree = response.result.testTree;
    assert.deepEqual(Object.keys(tree.workspaces).sort(), ['empty', 'inner', 'outer']);
    assert.deepEqual(tree.workspaces.empty.nodes, {});
    const outer = testsIn(tree.workspaces.outer.nodes);
    const inner = testsIn(tree.workspaces.inner.nodes);
    assert.equal(outer.length, 3);
    assert.deepEqual(outer.map(entry => entry.id).sort(), inner.map(entry => entry.id).sort());
    for (const entry of outer) {
      assert.equal(entry.workspaceId, 'outer');
      assert.equal(typeof entry.id, 'string');
      assert.equal(entry.status, 'undetermined');
      assert.equal(entry.isRunning, false);
      assert.equal(entry.isWaiting, false);
      assert.equal(entry.time, undefined);
      assert.equal(entry.group[0], 'nested');
      assert.equal(typeof entry.location.range.start.line, 'number');
      assert.equal(typeof entry.location.range.end.character, 'number');
    }
    assert.deepEqual((await client.request('fetch', { workspaces })).result.testTree, tree);
    assert.equal((await client.request('getTestTree', {})).error.code, -32601);
    assert.equal((await client.request('fetch', { workspaces: [{ id: 'missing', path: join(temporary, 'missing') }] })).error.code, -32602);
  });

  await context.test('empty selection, starts without status, terminal timing and workspace isolation', async () => {
    client.send('run', { testRuns: [{ workspacePath: project, testIds: [] }] });
    assert.equal((await client.request('fetch', { workspaces })).error, undefined);
    assert.equal(client.messages.some(message => message.method === 'testEvent'), false);
    const first = testsIn(tree.workspaces.outer.nodes).find(entry => entry.name === 'first');
    client.send('run', { testRuns: [{ workspacePath: project, testIds: [first.id] }, { workspacePath: nested, testIds: [first.id] }] });
    assert.equal((await client.request('fetch', { workspaces })).error.code, -32600);
    const events = await client.eventsUntilFinished(2);
    assert.equal(events.length, 4);
    for (const workspaceId of ['outer', 'inner']) {
      const selected = events.filter(event => event.payload.workspaceId === workspaceId);
      assert.deepEqual(selected[0].payload, { workspaceId, testId: first.id, isRunning: true });
      assert.equal(selected[1].payload.status, 'valid');
      assert.equal(selected[1].payload.isRunning, false);
      assert.equal(typeof selected[1].payload.time, 'number');
    }
  });

  await context.test('run-all and tooling errors do not produce fixtures', async () => {
    client.send('run', { testRuns: [{ workspacePath: project }] });
    const events = await client.eventsUntilFinished(3);
    assert.equal(events.filter(event => event.type === 'test-result').length, 0);
    assert.equal(events.filter(event => event.payload.status === 'valid').length, 2);
    assert.equal(events.filter(event => event.type === 'test-error').length, 1);
    client.send('run', { testRuns: [{ workspacePath: project, testIds: ['missing-id'] }] });
    assert.equal((await client.eventsUntilFinished(1))[0].type, 'test-error');
  });

  await context.test('real Blaster invalid-only fixture and later valid theorem', async () => {
    const examplePath = join(root, 'example');
    const response = await client.request('fetch', { workspaces: [{ id: 'example', path: examplePath }] });
    assert.equal(response.error, undefined);
    const examples = testsIn(response.result.testTree.workspaces.example.nodes);
    const invalid = examples.find(entry => entry.name === 'example_false_test');
    const valid = examples.find(entry => entry.name === 'example_full_test');
    assert.ok(invalid);
    assert.ok(valid);
    client.send('run', { testRuns: [{ workspacePath: examplePath, testIds: [invalid.id, valid.id] }] });
    const events = await client.eventsUntilFinished(2);
    const results = events.filter(event => event.type === 'test-result');
    assert.equal(results.length, 1, JSON.stringify(events));
    assert.equal(results[0].payload.testId, invalid.id);
    const fixture = results[0].payload.result;
    assert.deepEqual(Object.keys(fixture).sort(), ['graph', 'steps']);
    assert.equal(fixture.steps.length, 8);
    assert.equal(fixture.steps.filter(step => step.discarded).length, 4);
    assert.equal(fixture.graph.length, 2);
    assert.equal(new Set(fixture.graph.flatMap(tx => [...tx.inputs, ...tx.outputs].map(utxo => utxo.identifier))).size, 8);
    for (const entry of fixture.graph) {
      assert.equal(entry.tx.type, 'tx');
      assert.equal(entry.tx.id, entry.tx.identifier);
      assert.equal(entry.tx.inputCount, entry.inputs.length);
      assert.equal(entry.tx.outputCount, entry.outputs.length);
      for (const utxo of [...entry.inputs, ...entry.outputs]) {
        assert.equal(utxo.type, 'utxo');
        assert.equal(utxo.utxo, utxo.identifier);
        assert.equal(typeof utxo.value.lovelace, 'number');
        assert.ok(Array.isArray(utxo.value.assets));
      }
    }
    assert.equal(events.find(event => event.payload.testId === invalid.id && event.payload.status)?.payload.status, 'invalid');
    assert.equal(events.find(event => event.payload.testId === valid.id && event.payload.status)?.payload.status, 'valid');
    client.send('run', { testRuns: [{ workspacePath: examplePath, testIds: [valid.id] }] });
    assert.equal((await client.eventsUntilFinished(1)).some(event => event.type === 'test-result'), false);
  });

  await context.test('EOF exits without looping', async () => {
    const eofClient = new Client();
    await eofClient.wait(message => message.method === 'serverReady');
    eofClient.child.stdin.end();
    const timeout = setTimeout(() => eofClient.child.kill('SIGKILL'), 3000);
    const [code, signal] = await eofClient.exited;
    clearTimeout(timeout);
    assert.equal(signal, null);
    assert.equal(code, 0);
  });
});