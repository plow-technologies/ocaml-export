type newType =
  | NewType of int

let encodeNewType x =
  match x with
  | NewType y0 ->
     Json.Encode.int y0

let decodeNewType json =
  match Json.Decode.int json with
  | v -> Js_result.Ok (NewType v)
  | exception Json.Decode.DecodeError msg -> Js_result.Error ("decodeNewType: " ^ msg)
