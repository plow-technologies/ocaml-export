# Revision history for ocaml-export

## 0.3.0.0 -- 2018-01-04

* 'OCaml.BuckleScript.Encode.renderRef' now add parentheses when rendering: list, optional, either, pair, tuple3, tuple4, tuple5, tuple6.
* Support 'HasEmbeddedFile' for '(:<|>)'.
* 'HasDecoder' now unwraps all OCamlDatatype.

## 0.2.0.0 -- 2017-12-29

* `HasDecoder` properly supports Haskell types like `data X = X String Int`, product type with multiple unnamed records.

* Support `genericToOCamlDatatype` when one of the type's dependencies implements instance OCamlType without `genericToOCamlDatatype`.

## 0.1.1.0 -- 2017-12-19

* Remove `OCaml.File`. It was unexported, but was causing import errors because it depended on unincluded data files.
