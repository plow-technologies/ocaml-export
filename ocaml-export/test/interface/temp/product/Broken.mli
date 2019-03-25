type 'a0 innerBox =
  { ibA : 'a0
  ; ibS : string
  ; ibX : int
  }

val encodeInnerBox : ('a0 -> Js_json.t) -> 'a0 innerBox -> Js_json.t

val decodeInnerBox : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 innerBox, string) Belt.Result.t

type innerBox =
  | OuterBox of innerBox

val (encodeInnerBox) : outerBox -> Js_json.t

val (decodeInnerBox) : Js_json.t -> (outerBox, string) Belt.Result.t
