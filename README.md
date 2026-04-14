# afv-rpc-api

Lean4 JSON-RPC server for scanning workspaces and running theorem checks.

## What it does

The RPC server exposes a small JSON-RPC 2.0 API over stdio to:

- initialize one or more workspaces and scan for theorems
- return the full workspace tree (folders, files, theorems)
- run theorem checks and stream results

## Repository layout

- api/: Lean4 implementation of the RPC server and types
- example/: sample Lean workspace used for demos/tests
- demo/: scripts that exercise the RPC API over stdio
- scripts/: utility scripts (e.g., multi-arch build)

## Building on Windows

The server uses `Lake.loadWorkspace` at runtime to resolve Lean search paths. On Windows,
this requires the Lean interpreter to locate native symbols that are only available when the
binary is dynamically linked against the Lean shared libraries.

**Step 1** — uncomment `supportInterpreter` in `api/lakefile.toml`:

```toml
[[lean_exe]]
name = "afv-rpc-api"
root = "Main"
supportInterpreter = true   # uncomment this line
```

**Step 2** — build as normal:

```
cd api && lake build
```

**Step 3 (Windows only)** — copy the following DLLs from your Lean toolchain (e.g.
`~/.elan/toolchains/leanprover--lean4---v4.x.x/bin/`) alongside the compiled binary:

- `libleanshared.dll`
- `libleanshared_1.dll`
- `libleanshared_2.dll`
- `libLake_shared.dll`
- `libInit_shared.dll`
- `libc++.dll`
- `libclang-cpp.dll`
- `libLLVM-19.dll`
- `zlib1.dll`

## Quick start

Build the server:

- (cd api && lake build)

Run the demo (initializes the example workspace and prints the full tree):

- demo/init-workspace-demo.sh

## JSON-RPC methods (high level)

Notifications:

- initializeWorkspace { workspacePaths: string[] }
- updateWorkspace { workspacePaths: string[] }
- runTheorem string[]

Requests:

- getWorkspace -> Workspace[]
- getTheoremResult string -> TheoremResult