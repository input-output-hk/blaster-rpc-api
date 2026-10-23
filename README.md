# Blaster RPC API

Lean 4 JSON-RPC server for discovering and verifying Lean properties. The library is
`BlasterRpcApi`, the executable is `blaster-rpc-api`, and the sample project is
`BlasterExample`.

## Build and Verify

Install Elan/Lake and use the toolchain pinned in `api/lean-toolchain`. Run Lake
inside the project directory so Elan selects that toolchain:

```sh
cd api
lake build
cd ..
```

The executable is `api/.lake/build/bin/blaster-rpc-api`. Verification also needs
`lake` on PATH and the target project's dependencies and solver installed. Prepare
the example's Blaster dependency with `(cd example && lake build Blaster)`.
The example deliberately contains a false theorem, so building every example
module is expected to fail on that theorem.

With Node.js 22 or newer, run:

```sh
node --test scripts/rpc.test.mjs
demo/init-test-tree.sh
demo/run-basic-test.sh
demo/run-blaster-test.sh
demo/run-false-test.sh
demo/run-full-test.sh
```

The demos assert expected outcomes. The regression suite also covers empty and
nested workspaces, workspace isolation, stable IDs, run selections, failure
classification, framing, and shutdown. Set `BLASTER_RPC_BINARY` to test another
absolute executable path, including the extension's bundled binary.

## Wire Contract

Messages use JSON-RPC 2.0 with LSP `Content-Length` framing on stdin/stdout.
Diagnostics go to stderr. `serverReady` is sent at startup; an `exit` notification
or EOF terminates the process. Legacy methods are not supported.

### Discovery

Request `fetch` with:

```json
{"workspaces":[{"id":"example","path":"/absolute/path/to/project"}]}
```

The result is `{ "testTree": { "workspaces": { "example": workspace } } }`.
IDs are supplied by the client, nonempty, unique, and at most eight characters.
Paths must identify distinct existing directories. Empty workspaces are retained.
Refresh is rejected while tests are pending; successful refresh resets run state.

Each workspace has `id`, `path`, `status`, `isWaiting`, `isRunning`, `isOpen`,
optional `time`, and a `nodes` map. Folder nodes contain `nodes`; file nodes contain
a `tests` map. Nodes have `type`, `name`, and `isOpen`.

Tests contain string `id`, `workspaceId`, `name`, `group`, `location`, `status`,
`isWaiting`, `isRunning`, and optional `time`. Groups are workspace-relative path
segments ending with the filename. Locations contain `path` and
`range: {start: {line, character}, end: {line, character}}`, with zero-based lines
and UTF-16 character offsets. IDs are stable for unchanged path/name/line triples.
Discovery recognizes line-leading `theorem`, `#bmc`, and `#kind` declarations;
it is not a full Lean parser. Dependency, Git, and temporary verification files
are excluded.

### Execution

Send a `run` notification:

```json
{"testRuns":[{"workspacePath":"/absolute/path/to/project","testIds":["123"]}]}
```

Omit `testIds` to run all discovered tests in that workspace; `[]` runs none.
Duplicate or already-pending tests are not queued again. Execution is serialized.
Unknown test IDs emit `test-error`; unknown workspace paths are reported on stderr.

All test notifications use method `testEvent` and parameters
`{ "type": eventType, "payload": { "workspaceId": "example", "testId": "123", ... } }`:

| Type | Additional Payload | When |
| --- | --- | --- |
| `test-update` | `isRunning: true` (no `status`) | Verification starts |
| `test-result` | `result: {steps, graph}` | Confirmed invalid proof, before completion |
| `test-update` | `status: "valid"` or `"invalid"`, `isRunning: false`, `time` | Verification completes; time is real milliseconds |
| `test-error` | `error: {message, trace?}` | Tooling/execution/proof-processing failure; terminal, no fixture |

Absent optional fields are omitted, not `null`. Clients clear previous result
details when queuing a rerun. Public result details contain only `steps` and
`graph`; internal verifier diagnostics are not the public result format.

## Temporary Invalid Results

Verification status and timing are real. Counterexample details are **synthetic**,
provided by `api/BlasterRpcApi/InvalidResultFixture.lean` only after genuine
falsification. Valid tests and tooling errors receive no fixture.

The fixture preserves `MOCK_COUNTEREXAMPLE_STEPS` and `MOCK_GRAPH` from
`blaster-extension` commit `ab931282965bec6c7a0dcbb38d47c97e5e1d33f3`,
`src/services/testStore.ts`: eight steps (four discarded), two transactions, and
eight UTxOs. Two discarded hashes deliberately retain their original nonhex
characters. These are sample identifiers, not blockchain evidence.

The graph is converted to plain `GraphTx[]`: transaction/UTxO nodes, stable
identifiers, counts, consumed flags, lovelace/assets, and stringified datum data.
Unsupported historical fields are omitted. Replace this provider when actual
counterexample extraction is available; do not use it to infer proof outcomes.

## Distribution

Platform scripts and CI produce `blaster-rpc-api-{platform}-{arch}` artifacts
(`.exe` on Windows). Native build, demos, and protocol tests were verified on
macOS arm64. macOS x64, Linux, and Windows names are updated but their binaries
and runtime dependencies have not been verified in this migration.

For the extension, install the native executable at
`bin/blaster-rpc-api-darwin-arm64`. If stripping on macOS, use `xcrun strip` and
rerun the protocol suite against the installed binary.
