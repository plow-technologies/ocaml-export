type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

let encodeSuit x =
  match x with
  | Clubs ->
     Aeson.Encode.string "Clubs"
  | Diamonds ->
     Aeson.Encode.string "Diamonds"
  | Hearts ->
     Aeson.Encode.string "Hearts"
  | Spades ->
     Aeson.Encode.string "Spades"

let decodeSuit json =
  match Js_json.decodeString json with
  | Some "Clubs" -> Js_result.Ok Clubs
  | Some "Diamonds" -> Js_result.Ok Diamonds
  | Some "Hearts" -> Js_result.Ok Hearts
  | Some "Spades" -> Js_result.Ok Spades
  | Some err -> Js_result.Error ("decodeSuit: unknown enumeration '" ^ err ^ "'.")
  | None -> Js_result.Error "decodeSuit: expected a top-level JSON string."

type card =
  { cardSuit : suit
  ; cardValue : int
  }

let encodeCard x =
  Aeson.Encode.object_
    [ ( "cardSuit", encodeSuit x.cardSuit )
    ; ( "cardValue", Aeson.Encode.int x.cardValue )
    ]

let decodeCard json =
  match Aeson.Decode.
    { cardSuit = field "cardSuit" (fun a -> unwrapResult (decodeSuit a)) json
    ; cardValue = field "cardValue" int json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeCard: " ^ message)
