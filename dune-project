(lang dune 3.8)

(name compress)

(generate_opam_files true)

(source
 (github jkenda/compress))

(authors "Jakob Kenda")

(maintainers "Jakob Kenda")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name compress)
 (synopsis "A simple compression tool")
 (description "Compress files with Huffman, Lempel-Ziv or Deflate.")
 (depends ocaml dune bitv)
 (tags
  (compression huffman lz77 deflate)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
