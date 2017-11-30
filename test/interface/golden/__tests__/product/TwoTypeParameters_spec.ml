let () =
  AesonSpec.sampleGoldenAndServerSpec (TwoTypeParameters.decodeTwoTypeParameters (Aeson.Decode.wrapResult Aeson.Decode.int) (Aeson.Decode.wrapResult Aeson.Decode.int)) (TwoTypeParameters.encodeTwoTypeParameters Aeson.Encode.int Aeson.Encode.int) "twoTypeParameters" "http://localhost:8081/TwoTypeParameters/TwoTypeParameters" "golden/product/TwoTypeParameters.json";
