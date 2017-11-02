type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

val encodeSuit : suit -> Js_json.t

val decodeSuit : Js_json.t -> (suit, string) Js_result.t

type card =
  { cardSuit : suit
  ; cardValue : int
  }

val encodeCard : card -> Js_json.t

val decodeCard : Js_json.t -> (card, string) Js_result.t
