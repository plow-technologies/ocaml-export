type simpleChoice =
  { choice : (string, int) Aeson.Compatibility.Either.t
  }

val encodeSimpleChoice : simpleChoice -> Js_json.t

val decodeSimpleChoice : Js_json.t -> (simpleChoice, string) Js_result.t
