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

let decodeThree (type a0) (type a1) (type a2) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (decodeA1 : Js_json.t -> (a1, string) Js_result.t) (decodeA2 : Js_json.t -> (a2, string) Js_result.t) (json : Js_json.t) :((a0, a1, a2) three, string) Js_result.t =
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
