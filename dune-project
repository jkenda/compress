(lang dune 3.8)

(name huffman)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Jakob Kenda")

(maintainers "Jakob Kenda")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name huffman)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune bitv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
