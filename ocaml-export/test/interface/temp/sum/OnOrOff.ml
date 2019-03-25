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
  | Some "On" -> Belt.Result.Ok On
  | Some "Off" -> Belt.Result.Ok Off
  | Some err -> Belt.Result.Error ("decodeOnOrOff: unknown enumeration '" ^ err ^ "'.")
  | None -> Belt.Result.Error "decodeOnOrOff: expected a top-level JSON string."
