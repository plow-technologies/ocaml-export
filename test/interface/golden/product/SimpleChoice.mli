type simpleChoice =
  { choice : (int, string) Belt.Result.t
  }

val encodeSimpleChoice : simpleChoice -> Js_json.t

val decodeSimpleChoice : Js_json.t -> (simpleChoice, string) Belt.Result.t
