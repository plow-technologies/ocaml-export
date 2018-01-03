type nonGenericSumTypeInFile =
  | NGSumTypeA of string * int
  | NGSumTypeB of int * float

let encodeNonGenericSumTypeInFile x =
  match x with
  | NGSumTypeA (a,b) ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "NGSumTypeA" )
       ; ( "a", Aeson.Encode.string a )
       ; ( "b", Aeson.Encode.int a )
       ]     
  | NGSumTypeB (a,b) ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "NGSumTypeB" )
       ; ( "a", Aeson.Encode.int a )
       ; ( "b", Aeson.Encode.float a )
       ]

let decodeNonGenericSumTypeInFile json =
  match Aeson.Decode.(field "tag" string json) with
  | "NGSumTypeA" ->
     (match Aeson.Decode.(field "a" string json) with
      | a ->
         (match Aeson.Decode.(field "b" int json) with
          | b ->
             Js_result.Ok (NGSumTypeA (a, b))
          | exception Aeson.Decode.DecodeError message -> Js_result.Error ("NGSumTypeA expected b: " ^ message)
         )
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("NGSumTypeA expected a: " ^ message)
     )
  | "NGSumTypeB" ->
     (match Aeson.Decode.(field "a" int json) with
      | a ->
         (match Aeson.Decode.(field "b" Aeson.Decode.float json) with
          | b ->
             Js_result.Ok (NGSumTypeB (a, b))
          | exception Aeson.Decode.DecodeError message -> Js_result.Error ("NGSumTypeB expected b: " ^ message)
         )
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("NGSumTypeB expected a: " ^ message)
     )
  with
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Js_result.Error message
