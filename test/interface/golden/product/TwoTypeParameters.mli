type ('a0, 'a1) twoTypeParameters =
  { ttpId : int
  ; ttpFirst : 'a0
  ; ttpSecond : 'a1
  ; ttpThird : ('a0 * 'a1)
  }

val encodeTwoTypeParameters : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a0, 'a1) twoTypeParameters -> Js_json.t

val decodeTwoTypeParameters : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1) twoTypeParameters, string) Belt.Result.t
