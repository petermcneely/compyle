(lang dune 3.4)

(name compyle)

(generate_opam_files true)

(source
 (github petermcneely/compyle))

(authors "Peter McNeely" "Jacky Wong" "Ryan Grossman" "Zachary Hine")

(package
 (name compyle)
 (synopsis "A small, compiled language with Python-like syntax")
 (description "A small, compiled language with Python-like syntax")
 (depends ocaml dune)
 (tags
  ("plt")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
