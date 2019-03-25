type nonGenericType =
  { ngA : string
  ; ngB : int
  }

val encodeNonGenericType : nonGenericType -> Js_json.t

val decodeNonGenericType : Js_json.t -> (nonGenericType, string) Belt.Result.t
