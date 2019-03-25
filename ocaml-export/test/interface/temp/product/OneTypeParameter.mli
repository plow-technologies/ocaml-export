type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

val encodeOneTypeParameter : ('a0 -> Js_json.t) -> 'a0 oneTypeParameter -> Js_json.t

val decodeOneTypeParameter : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 oneTypeParameter, string) Belt.Result.t
