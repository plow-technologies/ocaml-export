type complexProduct =
  { cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  }

val encodeComplexProduct : complexProduct -> Js_json.t

val decodeComplexProduct : Js_json.t -> (complexProduct, string) Js_result.t
