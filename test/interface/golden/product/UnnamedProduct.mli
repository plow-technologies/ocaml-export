type unnamedProduct =
  | UnnamedProduct of string * int

val encodeUnnamedProduct : unnamedProduct -> Js_json.t

val decodeUnnamedProduct : Js_json.t -> (unnamedProduct, string) Js_result.t
