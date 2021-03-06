

# ocaml-export

- Export Haskell types to OCaml BuckleScript. 
- Export Haskell aeson serializations to OCaml BuckleScript using bs-aeson.
- Automate integration tests between OCaml BuckleScript and Haskell using bs-aeson-spec, quickcheck-arbitrary-adt, hspec-golden-aeson and servant.

## Test

```
bash test.sh
```

## Todo

Complete before stablizing and pushing to version 1.0.0.0.

- Documentation
- Examples
- readthedocs
- More tests.
- Haskell list as OCaml array.
- Support (Map String ..) (Map Text ..) (Map Int ..), must provide one decoder.
- Support (Map x ..) but two encoders/decoders must be provided.
- Resolve dependency order for output code so you are not required to declare 'OCamlPackage' types in the correct order.
- Suppport list of list, option of option, etc.
- Use GHC.TypeList (TypeError). 
- Correct Haskell Int (64-bits) to OCaml int, int32, int64 types.
- Support recursive and mutually interdependent types.

## Wishlist

Nice to have but low priority.

- OCaml source code parser and AST. Would make it a lot easier to handle manually written types, encoders, decoders.
- Output to js_of_ocaml and regular OCaml.

## ocaml-export Internal Tests

If you want to run it against servant:

```bash
stack test --flag ocaml-export:servant-spec
cd test/interface/golden
npm install
npm run test
```
