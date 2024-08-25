# JSON-RPC 2.0

This repository features a collection of packages to use the JSON-RPC 2.0
protocol in OCaml projects, both for servers and clients.

- `jsonrpc2-api` provides a way to describe JSON-RPC 2.0 methods, along with
  the data structures defined in the [JSON-RPC 2.0 Specification][specs].
- `jsonrpc2-server-lwt` implements a Lwt-based handler for JSON-RPC 2.0 methods
  defined with `jsonrpc2-api`.
- `jsonrpc2-dream` exports a `route` to be used in a [Dream]-based application.
- `jsonrpc2-http-client-lwt` provides the utility functions for performing
  procedure calls of methods defined with `jsonrpc2-api` and exposed by a
  remote HTTP server.

You can see a minimal example in `example/main.ml`.

[specs]: https://www.jsonrpc.org/specification
[Dream]: https://aantron.github.io/dream/
