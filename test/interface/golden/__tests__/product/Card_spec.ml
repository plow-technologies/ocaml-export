let () =
  AesonSpec.sampleGoldenAndServerSpec Card.decodeSuit Card.encodeSuit "suit" "http://localhost:8081/suit" "__tests__/golden/product/Suit.json";

  AesonSpec.sampleGoldenAndServerSpec Card.decodeCard Card.encodeCard "card" "http://localhost:8081/card" "__tests__/golden/product/Card.json";
