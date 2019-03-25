type simple =
  { sa : int
  ; sb : string
  }

val encodeSimple : simple -> Js_json.t

val decodeSimple : Js_json.t -> (simple, string) Belt.Result.t
