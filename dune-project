(lang dune 3.0)
(name jsonrpc2)
(generate_opam_files true)

(source
 (github lthms/jsonrpc2))

(license mit)
(authors "Thomas Letan <lthms@soap.coffee>")
(maintainers "Thomas Letan <lthms@soap.coffee>")
(version 0.1-dev)

(package
 (name jsonrpc2-api)
 (synopsis "Specify your JSON-RPC 2.0 API")
 (depends (ocaml (>= "4.08")) (ezjsonm-encoding (>= "2.1"))))

(package
 (name jsonrpc2-dream)
 (synopsis "Implement your JSON-RPC 2.0 on top of dream")
 (depends (ocaml (>= "4.08")) (dream (= "1.0.0~alpha6")) (ezjsonm-encoding (>= "2.1")) jsonrpc2-api))

(package
 (name jsonrpc2-client)
 (synopsis "Query your JSON-RPC 2.0 API")
 (depends (ocaml (>= "4.08")) http-lwt-client (ezjsonm-encoding (>= "2.1")) jsonrpc2-api))

(package
 (allow_empty)
 (name jsonrpc2-dev)
 (synopsis
  "Virtual package gathering developers dependencies for hacking the jsonrpc2 project")
 (depends
  dune-release
  ocaml-lsp-server
  odoc
  (ocamlformat
   (= 0.26.1))))
