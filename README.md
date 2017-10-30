# ocaml-export

Export Haskell types to OCaml. 

Export aeson serializations to OCaml BuckleScript.

## Goal

Encode Haskell types into OCaml/BuckleScript from an aeson perspective. 

## aeson

ignore functions

sum of records

newtype
data
sum
product with and without fields
enumeration (sum of named types only, the types hold no values)


## Variable Naming

- Type parameters: a0, a1, a2, etc.
- Tuple variables: t0, t1, t2, etc.
- List unwrapper: l
- Unrwap Type Parameter Decoder Result: a
- Encode Variable: x
