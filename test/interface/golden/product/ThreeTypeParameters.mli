type ('a0, 'a1, 'a2) three =
  { threeId : int
  ; threeFirst : 'a0
  ; threeSecond : 'a1
  ; threeThird : 'a2
  ; threeString : string
  }

val encodeThree : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) three -> Js_json.t

val decodeThree : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> (Js_json.t -> ('a2, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1, 'a2) three, string) Belt.Result.t
