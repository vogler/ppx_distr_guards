(lang dune 2.3)
(name ppx_distr_guards)
(allow_approximate_merlin)
(generate_opam_files true)

(source (github vogler/ppx_distr_guards))
(license MIT)
(authors "Ralf Vogler")
(maintainers "ralf.vogler@gmail.com")

(package
 (name ppx_distr_guards)
 (synopsis "Extension to distribute guards over or-patterns")
 (description "`function%distr A x, _ | _, A x when p x -> e` will result in `function A x, _ when p x -> e | _, A x when p x -> e`")
 (documentation "https://vogler.github.io/ppx_distr_guards")
 (depends
  (dune (>= 2.3.0))
  (ocaml-migrate-parsetree (>= 1.6.0))))
