(lang dune 3.15)
(using ctypes 0.3)

(license ISC)
(authors "The tsdl programmers")
(maintainers "Daniel Bünzli <daniel.buenzl i@erratique.ch>")
(source (uri "git+https://erratique.ch/repos/tsdl.git"))
(homepage "https://erratique.ch/software/tsdl")
(bug_reports "https://github.com/dbuenzli/tsdl/issues")

(package
 (name tsdl)
 (synopsis "Thin bindings to SDL for OCaml")
 (description
  "\| Tsdl is an OCaml library providing thin bindings to the cross-platform
  "\| [SDL library].
  "\|
  "\| Tsdl depends on the C library SDL 2.0.18 (or later),
  "\| [ocaml-ctypes][ctypes]. Tsdl is distributed under the ISC license.
  "\|
  "\| [SDL library]: https://www.libsdl.org/
  "\| [ctypes]: https://github.com/ocamllabs/ocaml-ctypes
 )
 (documentation "https://erratique.ch/software/tsdl/doc/")
 (tags ("audio" "bindings" "graphics" "media" "opengl" "input" "hci"
        "org:erratique"))
 (depends
  (ocaml (>= 4.08.0))
  conf-sdl2
  (ctypes (>= 0.21.1))
  (ctypes-foreign (>= 0.21.1))))

(generate_opam_files)
