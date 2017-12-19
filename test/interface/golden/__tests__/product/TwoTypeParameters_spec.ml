let () =
  AesonSpec.sampleGoldenAndServerSpec
    (TwoTypeParameters.decodeTwoTypeParameters AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (TwoTypeParameters.encodeTwoTypeParameters Aeson.Encode.int Aeson.Encode.int)
    "twoTypeParameters"
    "http://localhost:8081/TwoTypeParameters/TwoTypeParameters"
    "golden/product/TwoTypeParameters";
