let () =
  AesonSpec.sampleGoldenAndServerSpec (OneTypeParameter.decodeOneTypeParameter (Aeson.Decode.wrapResult Aeson.Decode.int)) (OneTypeParameter.encodeOneTypeParameter Aeson.Encode.int) "oneTypeParameter" "http://localhost:8081/OneTypeParameter/OneTypeParameter" "golden/product/OneTypeParameter.json";
