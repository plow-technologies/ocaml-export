type ('a0, 'a1) twoTypeParameters =
  { ttpId : int
  ; ttpFirst : 'a0
  ; ttpSecond : 'a1
  }

let encodeTwoTypeParameters encodeA0 encodeA1 x =
  Json.Encode.object_
    [ ( "ttpId", Json.Encode.int x.ttpId )
    ; ( "ttpFirst", encodeA0 x.ttpFirst )
    ; ( "ttpSecond", encodeA1 x.ttpSecond )
    ]

let decodeTwoTypeParameters decodeA0 decodeA1 json =
  match Json.Decode.
    { ttpId = field "ttpId" int json
    ; ttpFirst = field "ttpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    ; ttpSecond = field "ttpSecond" (fun a -> unwrapResult (decodeA1 a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeTwoTypeParameters: " ^ message)
