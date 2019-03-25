type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

val encodeSuit : suit -> Js_json.t

val decodeSuit : Js_json.t -> (suit, string) Belt.Result.t

type card =
  { cardSuit : suit
  ; cardValue : int
  }

val encodeCard : card -> Js_json.t

val decodeCard : Js_json.t -> (card, string) Belt.Result.t
