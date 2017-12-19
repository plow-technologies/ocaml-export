let () =
  AesonSpec.sampleGoldenAndServerSpec
    (Result.decodeResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Result.encodeResult Aeson.Encode.int Aeson.Encode.int)
    "result"
    "http://localhost:8082/Result/Result"
    "golden/sum/Result";
