type ('a0, 'a1, 'a2) three =
  { threeId : int
  ; threeFirst : 'a0
  ; threeSecond : 'a1
  ; threeThird : 'a2
  ; threeString : string
  }

let encodeThree (type a0) (type a1) (type a2) (encodeA0 : a0 -> Js_json.t) (encodeA1 : a1 -> Js_json.t) (encodeA2 : a2 -> Js_json.t) (x : (a0, a1, a2) three) :Js_json.t =
  Json.Encode.object_
    [ ( "threeId", Json.Encode.int x.threeId )
    ; ( "threeFirst", encodeA0 x.threeFirst )
    ; ( "threeSecond", encodeA1 x.threeSecond )
    ; ( "threeThird", encodeA2 x.threeThird )
    ; ( "threeString", Json.Encode.string x.threeString )
    ]
