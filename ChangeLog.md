# Revision history for ocaml-export

## 0.1.1.0 -- 2017-12-19

* Remove `OCaml.File`. It was unexported, but was causing import errors because it depended on unincluded data files.
