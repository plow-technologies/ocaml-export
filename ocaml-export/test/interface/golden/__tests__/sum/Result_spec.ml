let () =
  AesonSpec.goldenDirSpec
    (Result.decodeResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Result.encodeResult Aeson.Encode.int Aeson.Encode.int)
    "result"
    "golden/sum/Result";
