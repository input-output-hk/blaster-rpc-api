import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const workspacePath = join(root, 'example');
const [file, name, expectedStatus = 'valid'] = process.argv.slice(2);
const child = spawn(join(root, 'api/.lake/build/bin/blaster-rpc-api'), [], { cwd: join(root, 'api') });
const exited = once(child, 'exit');
child.stderr.pipe(process.stderr);
const timeout = setTimeout(() => child.kill('SIGKILL'), 120000);
let buffer = Buffer.alloc(0);
let finished = false;
let started = false;
let receivedResult = false;

function send(method, params, id) {
  const body = JSON.stringify({ jsonrpc: '2.0', method, params, ...(id === undefined ? {} : { id }) });
  child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
}

function testsIn(nodes) {
  return Object.values(nodes).flatMap(node => node.type === 'file' ? Object.values(node.tests) : testsIn(node.nodes));
}

try {
  send('fetch', { workspaces: [{ id: 'example', path: workspacePath }] }, 1);
  for await (const chunk of child.stdout) {
    buffer = Buffer.concat([buffer, chunk]);
    while (true) {
      const headerEnd = buffer.indexOf('\r\n\r\n');
      if (headerEnd < 0) break;
      const match = /^Content-Length: (\d+)/im.exec(buffer.subarray(0, headerEnd).toString());
      assert.ok(match, 'Invalid RPC framing');
      const end = headerEnd + 4 + Number(match[1]);
      if (buffer.length < end) break;
      const message = JSON.parse(buffer.subarray(headerEnd + 4, end).toString());
      buffer = buffer.subarray(end);
      console.log(JSON.stringify(message, null, 2));
      assert.equal(message.error, undefined, JSON.stringify(message.error));
      if (message.id === 1) {
        if (!file) finished = true;
        else {
          const selected = testsIn(message.result.testTree.workspaces.example.nodes)
            .find(entry => entry.name === name && entry.location.path.endsWith(`/BlasterExample/${file}`));
          assert.ok(selected, `Test not found: ${file}: ${name}`);
          send('run', { testRuns: [{ workspacePath, testIds: [selected.id] }] });
        }
      }
      if (message.method === 'testEvent') {
        const { type, payload } = message.params;
        assert.notEqual(type, 'test-error', JSON.stringify(payload));
        if (type === 'test-update' && payload.isRunning) {
          assert.equal(payload.status, undefined);
          started = true;
        }
        if (type === 'test-result') {
          assert.equal(expectedStatus, 'invalid');
          assert.ok(payload.result.steps.length && payload.result.graph.length);
          receivedResult = true;
        }
        if (type === 'test-update' && payload.status) {
          assert.ok(started);
          assert.equal(payload.status, expectedStatus);
          assert.equal(typeof payload.time, 'number');
          assert.equal(receivedResult, expectedStatus === 'invalid');
          finished = true;
        }
      }
      if (finished) send('exit', {});
    }
  }
  const [code, signal] = await exited;
  assert.ok(finished, 'Server closed before the demo completed');
  assert.equal(signal, null);
  assert.equal(code, 0);
} finally {
  clearTimeout(timeout);
  if (child.exitCode === null) child.kill();
}