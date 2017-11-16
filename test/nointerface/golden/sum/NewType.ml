type newType =
  | NewType of int

let encodeNewType (x : newType) :Js_json.t =
  match x with
  | NewType y0 ->
     Aeson.Encode.int y0

let decodeNewType (json : Js_json.t) :(newType, string) Js_result.t =
  match Aeson.Decode.int json with
  | v -> Js_result.Ok (NewType v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeNewType: " ^ msg)
