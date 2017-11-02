type withTuple =
  | WithTuple of (int * int)

val encodeWithTuple : withTuple -> Js_json.t

val decodeWithTuple : Js_json.t -> (withTuple, string) Js_result.t
