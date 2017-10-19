type ('a0, 'a1, 'a2) three =
  { threeId : int
  ; threeFirst : 'a0
  ; threeSecond : 'a1
  ; threeThird : 'a2
  ; threeString : string
  }

let encodeThree (type a0) (type a1) (type a2) (parseA0 : a0 -> Js_json.t) (parseA1 : a1 -> Js_json.t) (parseA2 : a2 -> Js_json.t) (x : (a0, a1, a2) three) :Js_json.t =
  Json.Encode.object_
    [ ( "threeId", Json.Encode.int x.threeId )
    ; ( "threeFirst", parseA0 x.threeFirst )
    ; ( "threeSecond", parseA1 x.threeSecond )
    ; ( "threeThird", parseA2 x.threeThird )
    ; ( "threeString", Json.Encode.string x.threeString )
    ]
