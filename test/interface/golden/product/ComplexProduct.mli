type complexProduct =
  { cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  ; cp2 : ((int) list) list
  ; cp3 : ((int) list) option
  }

val encodeComplexProduct : complexProduct -> Js_json.t

val decodeComplexProduct : Js_json.t -> (complexProduct, string) Js_result.t
