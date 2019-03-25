let () =
  AesonSpec.sampleGoldenAndServerSpec
    (OneTypeParameter.decodeOneTypeParameter AesonSpec.decodeIntWithResult)
    (OneTypeParameter.encodeOneTypeParameter Aeson.Encode.int)
    "oneTypeParameter"
    "http://localhost:8081/OneTypeParameter/OneTypeParameter"
    "golden/product/OneTypeParameter";
