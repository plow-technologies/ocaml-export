let () =
  AesonSpec.sampleGoldenAndServerSpec (ThreeTypeParameters.decodeThree (Aeson.Decode.wrapResult Aeson.Decode.int) (Aeson.Decode.wrapResult Aeson.Decode.int) (Aeson.Decode.wrapResult Aeson.Decode.int)) (ThreeTypeParameters.encodeThree Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int) "three" "http://localhost:8081/ThreeTypeParameters/Three" "golden/product/Three.json";
