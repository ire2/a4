(lang dune 3.16)

(name a4)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ignacio")

(maintainers "Ignacio")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name a4)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
