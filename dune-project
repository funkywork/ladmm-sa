(lang dune 3.0)

(name ladmm)
(version dev)
(generate_opam_files true)

(source (github funkywork/ladmm-sa))
(license GPL-3.0-only)
(authors "The Funkyworkers")
(maintainers "The Funkyworkers")

(package
  (name ladmm)
  (synopsis "The source code of the LADMM SA computer")
  (description
    "A small application that makes it easy to enter
    and track data to qualify for artist status in Belgium.
    It is a pro bono application designed at the request of
    'Les Amis d'ma mère', a Belgian non-profit organisation
    that promotes (and supports) artists in Belgium")
  (allow_empty)
  (depends
    (ocaml (>= 5.0.0))
    (dune (>= 3.0.0))
    (js_of_ocaml-compiler (>= 5.1.1))
    (js_of_ocaml-ppx (>= 5.1.1))
    ocaml-vdom
    (alcotest :with-test)))
