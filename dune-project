(lang dune 3.10)

(name caliper)

(generate_opam_files true)

(source
 (github tmattio/caliper))

(authors "Thibaut Mattio")

(maintainers "Thibaut Mattio")

(license ISC)

(using directory-targets 0.1)

(package
 (name caliper)
 (synopsis "Benchmarking toolchain and library for OCaml")
 (description "Benchmarking toolchain and library for OCaml.")
 (depends sexplib ppx_sexp_conv cmdliner mustache tailwindcss yojson ISO8601))
