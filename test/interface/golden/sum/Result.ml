type ('a0, 'a1) result =
  | Success of 'a0
  | Error of 'a1

let encodeResult encodeA0 encodeA1 x =
  match x with
  | Success y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "Success" )
       ; ( "contents", encodeA0 y0 )
       ]
  | Error y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "Error" )
       ; ( "contents", encodeA1 y0 )
       ]

let decodeResult decodeA0 decodeA1 json =
  match Aeson.Decode.(field "tag" string json) with
  | "Success" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeA0 a)) json) with
      | v -> Belt.Result.Ok (Success v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Success: " ^ message)
     )
  | "Error" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeA1 a)) json) with
      | v -> Belt.Result.Ok (Error v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Error: " ^ message)
     )
  | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message
