(lang dune 2.7)
(name ppx_distr_guards)
(allow_approximate_merlin)
(generate_opam_files true)

(source (github vogler/ppx_distr_guards))
(documentation "https://vogler.github.io/ppx_distr_guards")
(authors "Ralf Vogler <ralf.vogler@gmail.com>")
(maintainers "Ralf Vogler <ralf.vogler@gmail.com>")
(license MIT)

(package
  (name ppx_distr_guards)
  (synopsis "Extension to distribute guards over or-patterns")
  (description "`function%distr A x, _ | _, A x when p x -> e` will result in `function A x, _ when p x -> e | _, A x when p x -> e`")
  (depends
   (ppxlib (>= 0.15.0))
   (odoc :with-doc)
  )
)
