(*
val xxxx : int


type ('a0, 'a1) subTypeParameter =
  { listA : ('a0) list
  ; maybeB : ('a1) option
  }

let encodeSubTypeParameter2 encodeA0 encodeA1 x =

val encodeSubTypeParameter2 : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a0, 'a1) subTypeParameter -> Js_json.t

let decodeSubTypeParameter2 encodeA0 encodeA1 json =

val decodeSubTypeParameter2 : (Js_json.t -> ('a0, string) Js_result.t) -> (Js_json.t -> ('a1, string) Js_result.t) -> Js_json.t -> (('a0,'a1) subTypeParameter, string) Js_result.t
                                                                                                                                   *)
