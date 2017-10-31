type nameOrIdNumber =
  | Name of string
  | IdNumber of int

let encodeNameOrIdNumber (x : nameOrIdNumber) :Js_json.t =
  match x with
  | Name y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "Name" )
       ; ( "contents", Json.Encode.string y0 )
       ]
  | IdNumber y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "IdNumber" )
       ; ( "contents", Json.Encode.int y0 )
       ]

let decodeNameOrIdNumber (json : Js_json.t) :(nameOrIdNumber, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "Name" ->
     (match Json.Decode.(field "contents" string json) with
      | v -> Js_result.Ok (Name v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("Name: " ^ message)
     )
  | "IdNumber" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (IdNumber v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("IdNumber: " ^ message)
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error message
