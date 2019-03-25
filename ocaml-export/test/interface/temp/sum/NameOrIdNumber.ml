type nameOrIdNumber =
  | Name of string
  | IdNumber of int

let encodeNameOrIdNumber x =
  match x with
  | Name y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "Name" )
       ; ( "contents", Aeson.Encode.string y0 )
       ]
  | IdNumber y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "IdNumber" )
       ; ( "contents", Aeson.Encode.int y0 )
       ]

let decodeNameOrIdNumber json =
  match Aeson.Decode.(field "tag" string json) with
  | "Name" ->
     (match Aeson.Decode.(field "contents" string json) with
      | v -> Belt.Result.Ok (Name v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Name: " ^ message)
     )
  | "IdNumber" ->
     (match Aeson.Decode.(field "contents" int json) with
      | v -> Belt.Result.Ok (IdNumber v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("IdNumber: " ^ message)
     )
  | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message
