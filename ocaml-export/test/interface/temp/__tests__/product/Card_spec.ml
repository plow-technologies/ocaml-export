let () =
  AesonSpec.goldenDirSpec
    Card.decodeSuit
    Card.encodeSuit
    "suit"
    "golden/product/Suit";

  AesonSpec.goldenDirSpec
    Card.decodeCard
    Card.encodeCard
    "card"
    "golden/product/Card";
