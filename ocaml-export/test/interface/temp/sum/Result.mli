type ('a0, 'a1) result =
  | Success of 'a0
  | Error of 'a1

val encodeResult : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a0, 'a1) result -> Js_json.t

val decodeResult : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1) result, string) Belt.Result.t
