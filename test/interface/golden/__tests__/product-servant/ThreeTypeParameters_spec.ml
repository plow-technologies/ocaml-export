let () =
  AesonSpec.sampleGoldenAndServerSpec
    (ThreeTypeParameters.decodeThree AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (ThreeTypeParameters.encodeThree Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "three"
    "http://localhost:8081/ThreeTypeParameters/Three"
    "golden/product/Three";
