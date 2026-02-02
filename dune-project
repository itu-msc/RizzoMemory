(lang dune 3.20)

(name rizzoc)

(generate_opam_files true)

(source
 (github itu-msc/RizzoMemory))

(maintainers "Patrick Matthiesen" "tjobi")

(authors "Patrick Matthiesen" "tjobi")

(license MIT)

(documentation https://github.com/itu-msc/RizzoMemory/wiki)

(using menhir 3.0)

(package
 (name rizzoc)
 (synopsis "Rizzo memory model")
 (description "A longer description")
 (depends ocaml menhir dune)
 (tags
  ("FRP"
   "functional reactive programming")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
