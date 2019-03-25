type nameOrIdNumber =
  | Name of string
  | IdNumber of int

val encodeNameOrIdNumber : nameOrIdNumber -> Js_json.t

val decodeNameOrIdNumber : Js_json.t -> (nameOrIdNumber, string) Js_result.t
