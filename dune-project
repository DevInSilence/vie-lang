(lang dune 3.16)

(name vie)

(generate_opam_files true)

(source
 (github devinsilence/vie)) 

(authors "DevInSilence")

(maintainers "DevInSilence")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name vie)
 (synopsis "Vie is a small compiled programming language")
 (description "Vie is programming language which was developed while learning OCaml")
 (depends ocaml dune camomile)
 (tags
  (topics "programming" "languages")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
