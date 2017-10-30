type ('a0, 'a1) result =
  | Success of 'a0
  | Error of 'a1

let encodeResult (type a0) (type a1) (encodeA0 : a0 -> Js_json.t) (encodeA1 : a1 -> Js_json.t) (x : (a0, a1) result) :Js_json.t =
  match x with
  | Success y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "Success" )
       ; ( "contents", encodeA0 y0 )
       ]
  | Error y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "Error" )
       ; ( "contents", encodeA1 y0 )
       ]

let decodeResult (type a0) (type a1) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (decodeA1 : Js_json.t -> (a1, string) Js_result.t) (json : Js_json.t) :((a0, a1) result, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "Success" ->
     (match Json.Decode.(field "contents" (fun a -> unwrapResult (decodeA0 a)) json) with
      | v -> Js_result.Ok (Success v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("Success: " ^ message)
     )
  | "Error" ->
     (match Json.Decode.(field "contents" (fun a -> unwrapResult (decodeA1 a)) json) with
      | v -> Js_result.Ok (Error v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("Error: " ^ message)
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error message
