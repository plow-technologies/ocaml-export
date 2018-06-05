type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)
  | HasMixed of int * string * float

val encodeSumVariant : sumVariant -> Js_json.t

val decodeSumVariant : Js_json.t -> (sumVariant, string) Belt.Result.t
