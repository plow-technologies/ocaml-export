type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

let encodeOneTypeParameter (type a0) (encodeA0 : a0 -> Js_json.t) (x : a0 oneTypeParameter) :Js_json.t =
  Aeson.Encode.object_
    [ ( "otpId", Aeson.Encode.int x.otpId )
    ; ( "otpFirst", encodeA0 x.otpFirst )
    ]

let decodeOneTypeParameter (type a0) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (json : Js_json.t) :(a0 oneTypeParameter, string) Js_result.t =
  match Aeson.Decode.
    { otpId = field "otpId" int json
    ; otpFirst = field "otpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeOneTypeParameter: " ^ message)
