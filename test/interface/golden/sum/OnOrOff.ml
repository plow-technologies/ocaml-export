type onOrOff =
  | On
  | Off

let encodeOnOrOff x =
  match x with
  | On ->
     Aeson.Encode.string "On"
  | Off ->
     Aeson.Encode.string "Off"

let decodeOnOrOff json =
  match Js_json.decodeString json with
  | Some "On" -> Js_result.Ok On
  | Some "Off" -> Js_result.Ok Off
  | Some err -> Js_result.Error ("decodeOnOrOff: unknown enumeration '" ^ err ^ "'.")
  | None -> Js_result.Error "decodeOnOrOff: expected a top-level JSON string."
