[![CircleCI](https://circleci.com/gh/plow-technologies/ocaml-export.svg?style=shield&circle)](https://circleci.com/gh/plow-technologies/ocaml-export)

# ocaml-export

- Export Haskell types to OCaml BuckleScript. 
- Export Haskell aeson serializations to OCaml BuckleScript using bs-aeson.
- Automate integration tests between OCaml BuckleScript and Haskell using bs-aeson-spec, quickcheck-arbitrary-adt, hspec-golden-aeson and servant.

## Test

```
bash test.sh
```

## Todo

Complete before stablizing.

- Documentation
- Examples
- readthedocs
- More tests.
- Haskell list as OCaml array.
- Support (Map String ..) (Map Text ..) (Map Int ..), must provide one decoder.
- Support (Map x ..) but two encoders/decoders must be provided.
- Resolve dependency order for output code so you are not required to declare 'OCamlPackage' types in the correct order.
- suppport list of list, option of option, etc.
- Use GHC.TypeList (TypeError). 

## Wishlist

Nice to have but low priority.

- [ ] OCaml source code parser and AST. Would make it a lot easier to handle manually written types, encoders, decoders.
- [ ] Output to js_of_ocaml and regular OCaml.
