# Revision history for ocaml-export

## 0.13.0.0 -- 2019-03-17
* Expose TypeParamterRef constructors. They are useful in some cases.

## 0.12.0.0 -- 2018-12-28
* Fix Decode record case when NamedConstructor is an OCamlRef, not a primitive.

## 0.11.0.0 -- 2018-12-22
* Support record, encode, decode for type that has a type parameter filled with a non-primitive type (eg. newtype Z = Z (Maybe X))

## 0.10.0.0 -- 2018-12-07

* Support sum-type constructors with non-primitive types
* Remove dodgy-imports warnings with latests wl.pprint text
* addDependentFile for embedded files

## 0.9.0.0 -- 2018-06-03

* Output BuckleScript code requires at least bs-platform 3.1.0.
* Change output BuckleScript code and test code to use `Belt.Result.t` instead of `Js.Result.t`. `Js.Result.t` has been deprecated since bs-platform 3.1.0.
* Add `OInt32` to `OCaml.BuckleScript.Types.OCamlPrimitive` to convert Haskell `Int32` to BuckleScript `int32`.

## 0.8.0.0 -- 2018-05-03

* Support GHC 8.2.2 and drop support for previous GHC versions. 'base >= 4.10' is a requirement.
* Replace 'typelits-witnesses' dependency with 'singletons'.
* Remove use of Overlappable from 'HasEmbeddedFile'. Use 'Nat' flags instead.

## 0.7.0.0 -- 2018-02-14

* Add 'runGoldenSpec' to automate Haskell side file checks.
* Alter 'SpecOptions servantURL' to 'Maybe String'. If it is nothing then produce specs that use files, if Just then spec that uses servant and files.
* Fix encoding for sum type with enumerator.

## 0.6.0.0 -- 2018-01-31

* Properly convert Haskell types to OCaml types that have type parameters.

* Fix cases when the order of type parameter declarations differs from the order they appear in right hand side of the type declaration.

* Add 'template-haskell < 2.12.0.0' restriction. OCamlTypeInFile seems to break at runtime with version 2.12.0.0.

## 0.5.0.0 -- 2018-01-16

* Fix 'OCaml.BuckleScript.Encode.renderRef' for OBool.

## 0.4.0.0 -- 2018-01-10

* Add 'OCamlType' instance for ByteString.

* Add 'HaskellTypeName' to support servant test server for types without Generic. You must manually provide the type's name as a symbol.

## 0.3.0.0 -- 2018-01-04

* 'OCaml.BuckleScript.Encode.renderRef' now add parentheses when rendering: list, optional, either, pair, tuple3, tuple4, tuple5, tuple6.
* Support 'HasEmbeddedFile' for '(:<|>)'.
* 'HasDecoder' now unwraps all OCamlDatatype.

## 0.2.0.0 -- 2017-12-29

* `HasDecoder` properly supports Haskell types like `data X = X String Int`, product type with multiple unnamed records.

* Support `genericToOCamlDatatype` when one of the type's dependencies implements instance OCamlType without `genericToOCamlDatatype`.

## 0.1.1.0 -- 2017-12-19

* Remove `OCaml.File`. It was unexported, but was causing import errors because it depended on unincluded data files.
