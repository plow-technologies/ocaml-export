type onOrOff =
  | On
  | Off

let encodeOnOrOff (x : onOrOff) :Js_json.t =
  match x with
  | On ->
     Json.Encode.string "On"
  | Off ->
     Json.Encode.string "Off"
