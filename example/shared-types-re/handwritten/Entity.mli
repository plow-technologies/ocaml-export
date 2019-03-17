type ('key, 'value) entity =
  { entityKey   : 'key
  ; entityValue : 'value
  }

val encodeEntity : ('key -> Js_json.t) -> ('value -> Js_json.t) -> ('key, 'value) entity -> Js_json.t

val decodeEntity : (Js_json.t -> ('key, string) Belt.Result.t) -> (Js_json.t -> ('value, string) Belt.Result.t) -> Js_json.t -> (('key, 'value) entity, string) Belt.Result.t
