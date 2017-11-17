type nameOrIdNumber =
  | Name of string
  | IdNumber of int

let encodeNameOrIdNumber x =
  match x with
  | Name y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "name" )
       ; ( "contents", Aeson.Encode.string y0 )
       ]
  | IdNumber y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "idnumber" )
       ; ( "contents", Aeson.Encode.int y0 )
       ]

let decodeNameOrIdNumber json =
  match Aeson.Decode.(field "tag" string json) with
  | "name" ->
     (match Aeson.Decode.(field "contents" string json) with
      | v -> Js_result.Ok (Name v)
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("name: " ^ message)
     )
  | "idnumber" ->
     (match Aeson.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (IdNumber v)
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("idnumber: " ^ message)
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Js_result.Error message
