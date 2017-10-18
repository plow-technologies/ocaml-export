type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

let encodeSuit (x : suit) :Js_json.t =
  match x with
  | Clubs ->
     Json.Encode.string "Clubs"
  | Diamonds ->
     Json.Encode.string "Diamonds"
  | Hearts ->
     Json.Encode.string "Hearts"
  | Spades ->
     Json.Encode.string "Spades"

type card =
  { cardSuit : suit
  ; cardValue : int
  }

let encodeCard (x : card) :Js_json.t =
  Json.Encode.object_
    [ ( "cardSuit", encodeSuit x.cardSuit )
    ; ( "cardValue", Json.Encode.int x.cardValue )
    ]
