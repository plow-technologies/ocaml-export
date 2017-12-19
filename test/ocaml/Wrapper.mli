type ('a, 'b) wrapper =
  { wrapperA : 'a
  ; wrapperB : 'b
  ; wrapperC : string
  }

val encodeWrapper : ('a -> Js_json.t) -> ('b -> Js_json.t) -> ('a, 'b) wrapper -> Js_json.t

val decodeWrapper : (Js_json.t -> ('a, string) Js_result.t) -> (Js_json.t -> ('b, string) Js_result.t) -> Js_json.t -> (('a, 'b) wrapper, string) Js_result.t
