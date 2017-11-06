type withTuple =
  | WithTuple of (int * int)

let encodeWithTuple (x : withTuple) :Js_json.t =
  match x with
  | WithTuple y0 ->
     Json.Encode.pair Json.Encode.int Json.Encode.int y0

let decodeWithTuple (json : Js_json.t) :(withTuple, string) Js_result.t =
  match Json.Decode.(pair int int) json with
  | v -> Js_result.Ok (WithTuple v)
  | exception Json.Decode.DecodeError msg -> Js_result.Error ("decodeWithTuple: " ^ msg)
