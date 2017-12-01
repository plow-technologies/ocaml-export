# ocaml-export

Export Haskell types to OCaml. 

Export aeson serializations to OCaml BuckleScript.

## Todo

- [ ] Use internal modules
- [ ] Clean up OCaml.BuckleScript.Module
- [ ] Automate package files, commit bs dependencies to npm
- [ ] Documentation
- [ ] Examples
- [ ] readthedocs
- [ ] More tests
- [ ] Haskell list as OCaml array
- [ ] Support (Map String ..) (Map Text ..) (Map Int ..), must provide one decoder
- [ ] Support (Map x ..) but two encoders/decoders must be provided

## Test

```
stack build

cd test/interface/golden
npm install
npm run build

stack test

stack exec server

npm run test
```

## Goal

Encode Haskell types into OCaml/BuckleScript from an aeson perspective. 

## aeson

How aeson encodes Haskell data types.

A record is a product with named fields.

```
λ> :set -XDeriveAnyClass
λ> :set -XDeriveGeneric
λ> import Data.Aeson
λ> import GHC.Generics

λ> data Record =
  Record
    { rFirst :: String
    , rSecond :: Int
    } deriving (Generic,FromJSON,ToJSON)

λ> encode . toJSON $ Record "Hello" 123
"{\"rSecond\":123,\"rFirst\":\"Hello\"}"
```

An enumerator is a sum with with only tags, no values.

```
data Enumerator 
  = EFirst
  | ESecond
  | EThird
  deriving (Generic,FromJSON,ToJSON)

λ> encode . toJSON $ EFirst
"\"EFirst\""

λ> encode . toJSON $ ESecond
"\"ESecond\""
```

A product is a single constructor with values but no named fields.

```
data Product = 
  Product Int String
  deriving (Generic,FromJSON,ToJSON)
  
λ> encode . toJSON $ Product 1 "Hello"
"[1,\"Hello\"]"
```

A sum is a collection of products (no named fields) and enumerators.

```
data Sum 
  = Sum1 Int
  | Sum2 String
  | Sum3 (Int, String)
  | SumEnumerator
  deriving (Generic,FromJSON,ToJSON)

λ> encode . toJSON $ Sum1 1
"{\"tag\":\"Sum1\",\"contents\":1}"

λ> encode . toJSON $ Sum2 "Hello"
"{\"tag\":\"Sum2\",\"contents\":\"Hello\"}"

λ> encode . toJSON $ Sum3 (1, "Hello")
"{\"tag\":\"Sum3\",\"contents\":[1,\"Hello\"]}"

λ> encode . toJSON $ SumEnumerator
"{\"tag\":\"SumEnumerator\"}"
```

```
data SumWithRecord 
  = Sum4 Int
  | Person { name :: String, age :: Int}
  deriving (Generic,FromJSON,ToJSON)
  
λ> encode . toJSON $ Sum4 1
"{\"tag\":\"Sum4\",\"contents\":1}"

λ> encode . toJSON $ Person "Louis" 50
"{\"tag\":\"Person\",\"age\":50,\"name\":\"Louis\"}"
```


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
