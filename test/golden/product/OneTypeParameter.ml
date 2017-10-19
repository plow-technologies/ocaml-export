type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

let encodeOneTypeParameter (type a0) (parseA0 : a0 -> Js_json.t) (x : a0 oneTypeParameter) :Js_json.t =
  Json.Encode.object_
    [ ( "otpId", Json.Encode.int x.otpId )
    ; ( "otpFirst", parseA0 x.otpFirst )
    ]
