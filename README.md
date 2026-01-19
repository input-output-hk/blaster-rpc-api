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