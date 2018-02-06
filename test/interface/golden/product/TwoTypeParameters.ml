type ('a0, 'a1) twoTypeParameters =
  { ttpId : int
  ; ttpFirst : 'a0
  ; ttpSecond : 'a1
  ; ttpThird : ('a0 * 'a1)
  }

let encodeTwoTypeParameters encodeA0 encodeA1 x =
  Aeson.Encode.object_
    [ ( "ttpId", Aeson.Encode.int x.ttpId )
    ; ( "ttpFirst", encodeA0 x.ttpFirst )
    ; ( "ttpSecond", encodeA1 x.ttpSecond )
    ; ( "ttpThird", (Aeson.Encode.pair encodeA0 encodeA1) x.ttpThird )
    ]

let decodeTwoTypeParameters decodeA0 decodeA1 json =
  match Aeson.Decode.
    { ttpId = field "ttpId" int json
    ; ttpFirst = field "ttpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    ; ttpSecond = field "ttpSecond" (fun a -> unwrapResult (decodeA1 a)) json
    ; ttpThird = field "ttpThird" (pair (fun a -> unwrapResult (decodeA0 a)) (fun a -> unwrapResult (decodeA1 a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeTwoTypeParameters: " ^ message)
