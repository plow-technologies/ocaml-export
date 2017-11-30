let () =
  AesonSpec.sampleGoldenAndServerSpec (Result.decodeResult (Aeson.Decode.wrapResult Aeson.Decode.int) (Aeson.Decode.wrapResult Aeson.Decode.int)) (Result.encodeResult Aeson.Encode.int Aeson.Encode.int) "result" "http://localhost:8082/Result/Result" "golden/sum/Result.json";
