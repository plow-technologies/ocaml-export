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
  | Some "Clubs" -> Belt.Result.Ok Clubs
  | Some "Diamonds" -> Belt.Result.Ok Diamonds
  | Some "Hearts" -> Belt.Result.Ok Hearts
  | Some "Spades" -> Belt.Result.Ok Spades
  | Some err -> Belt.Result.Error ("decodeSuit: unknown enumeration '" ^ err ^ "'.")
  | None -> Belt.Result.Error "decodeSuit: expected a top-level JSON string."

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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeCard: " ^ message)
