type ('a0, 'a1, 'a2) subTypeParameter =
  { listA : ('a0) list
  ; maybeB : ('a1) option
  ; tupleC : ('a2 * 'a1)
  }

val encodeSubTypeParameter : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) subTypeParameter -> Js_json.t

val decodeSubTypeParameter : (Js_json.t -> ('a0, string) Js_result.t) -> (Js_json.t -> ('a1, string) Js_result.t) -> (Js_json.t -> ('a2, string) Js_result.t) -> Js_json.t -> (('a0, 'a1, 'a2) subTypeParameter, string) Js_result.t
