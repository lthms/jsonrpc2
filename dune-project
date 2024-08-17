(lang dune 3.0)

(name nightmare)

(generate_opam_files true)

(source
 (github lthms/nightmare))

(license mit)

(authors "Thomas Letan <lthms@soap.coffee>")

(maintainers "Thomas Letan <lthms@soap.coffee>")

(version 1-dev)

(package
 (name nightmare)
 (synopsis "JSON RPC API on top on Dream")
 (depends (ocaml (>= "4.08")) (dream (= "1.0.0~alpha6")) ezjsonm-encoding))

(package
 (allow_empty)
 (name nightmare-dev)
 (synopsis
  "Virtual package gathering developers dependencies for hacking the nightmare project")
 (depends
  dune-release
  ocaml-lsp-server
  odoc
  (ocamlformat
   (= 0.26.1))))
