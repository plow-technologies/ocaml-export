type ('a0, 'a1) twoTypeParameters =
  { ttpId : int
  ; ttpFirst : 'a0
  ; ttpSecond : 'a1
  }

let encodeTwoTypeParameters (type a0) (type a1) (encodeA0 : a0 -> Js_json.t) (encodeA1 : a1 -> Js_json.t) (x : (a0, a1) twoTypeParameters) :Js_json.t =
  Json.Encode.object_
    [ ( "ttpId", Json.Encode.int x.ttpId )
    ; ( "ttpFirst", encodeA0 x.ttpFirst )
    ; ( "ttpSecond", encodeA1 x.ttpSecond )
    ]

let decodeTwoTypeParameters (type a0) (type a1) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (decodeA1 : Js_json.t -> (a1, string) Js_result.t) (json : Js_json.t) :((a0, a1) twoTypeParameters, string) Js_result.t =
  match Json.Decode.
    { ttpId = field "ttpId" int json
    ; ttpFirst = field "ttpFirst" (fun a -> unwrapResult (decodeA0 a)) json
    ; ttpSecond = field "ttpSecond" (fun a -> unwrapResult (decodeA1 a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeTwoTypeParameters: " ^ message)
