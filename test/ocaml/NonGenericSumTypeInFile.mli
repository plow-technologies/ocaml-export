type nonGenericSumTypeInFile =
  | NGSumTypeA of string * int
  | NGSumTypeB of int * float

val encodeNonGenericSumTypeInFile : nonGenericSumTypeInFile -> Js_json.t

val decodeNonGenericSumTypeInFile : Js_json.t -> (nonGenericSumTypeInFile, string) Js_result.t
