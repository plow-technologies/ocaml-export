type onOrOff =
  | On
  | Off

val encodeOnOrOff : onOrOff -> Js_json.t

val decodeOnOrOff : Js_json.t -> (onOrOff, string) Js_result.t
