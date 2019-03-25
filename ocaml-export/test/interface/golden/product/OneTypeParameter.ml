type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

let encodeOneTypeParameter encodeA0 x =
  Aeson.Encode.object_
    [ ( "otpId", Aeson.Encode.int x.otpId )
    ; ( "otpFirst", encodeA0 x.otpFirst )
    ]

let decodeOneTypeParameter decodeA0 json =
  match Aeson.Decode.
    { otpId = field "otpId" int json
    ; otpFirst = field "otpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeOneTypeParameter: " ^ message)
