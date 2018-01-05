type withTuple =
  | WithTuple of (int * int)

let encodeWithTuple x =
  match x with
  | WithTuple y0 ->
     (Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int) y0

let decodeWithTuple json =
  match Aeson.Decode.(pair int int) json with
  | v -> Js_result.Ok (WithTuple v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeWithTuple: " ^ msg)
