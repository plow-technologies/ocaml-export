let () =
  AesonSpec.sampleGoldenAndServerSpec
    Card.decodeSuit
    Card.encodeSuit
    "suit"
    "http://localhost:8081/Card/Suit"
    "golden/product/Suit";

  AesonSpec.sampleGoldenAndServerSpec
    Card.decodeCard
    Card.encodeCard
    "card"
    "http://localhost:8081/Card/Card"
    "golden/product/Card";
