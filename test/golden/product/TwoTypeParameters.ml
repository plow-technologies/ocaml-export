type ( 'a0, 'a1 ) twoTypeParameters =
  { ttpId : int
  ; ttpFirst : 'a0
  ; ttpSecond : 'a1
  }

let encodeTwoTypeParameters (type a0) (type a1) (parseA0 : a0 -> Js_json.t) (parseA1 : a1 -> Js_json.t) (x : (a0, a1) twoTypeParameters) :Js_json.t =
  Json.Encode.object_
    [ ( "ttpId", Json.Encode.int x.ttpId )
    ; ( "ttpFirst", parseA0 x.ttpFirst )
    ; ( "ttpSecond", parseA1 x.ttpSecond )
    ]  
