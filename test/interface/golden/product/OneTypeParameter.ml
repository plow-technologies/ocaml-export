type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

let encodeOneTypeParameter encodeA0 x =
  Json.Encode.object_
    [ ( "otpId", Json.Encode.int x.otpId )
    ; ( "otpFirst", encodeA0 x.otpFirst )
    ]

let decodeOneTypeParameter decodeA0 json =
  match Json.Decode.
    { otpId = field "otpId" int json
    ; otpFirst = field "otpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeOneTypeParameter: " ^ message)
