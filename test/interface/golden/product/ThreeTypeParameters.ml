type ('a0, 'a1, 'a2) three =
  { threeId : int
  ; threeFirst : 'a0
  ; threeSecond : 'a1
  ; threeThird : 'a2
  ; threeString : string
  }

let encodeThree encodeA0 encodeA1 encodeA2 x =
  Json.Encode.object_
    [ ( "threeId", Json.Encode.int x.threeId )
    ; ( "threeFirst", encodeA0 x.threeFirst )
    ; ( "threeSecond", encodeA1 x.threeSecond )
    ; ( "threeThird", encodeA2 x.threeThird )
    ; ( "threeString", Json.Encode.string x.threeString )
    ]

let decodeThree decodeA0 decodeA1 decodeA2 json =
  match Json.Decode.
    { threeId = field "threeId" int json
    ; threeFirst = field "threeFirst" (fun a -> unwrapResult (decodeA0 a)) json
    ; threeSecond = field "threeSecond" (fun a -> unwrapResult (decodeA1 a)) json
    ; threeThird = field "threeThird" (fun a -> unwrapResult (decodeA2 a)) json
    ; threeString = field "threeString" string json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeThree: " ^ message)
