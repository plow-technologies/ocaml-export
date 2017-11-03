type newType =
  | NewType of int

val encodeNewType : newType -> Js_json.t

val decodeNewType : Js_json.t -> (newType, string) Js_result.t
