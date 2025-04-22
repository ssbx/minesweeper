(lang dune 3.15)
(using dune_site 0.1)

(name minesweeper)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name minesweeper)
 (sites (share images) (share grids))
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dune-site
          core
          caml-sdl2
          caml-sdl2-image
          conf-glew)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
