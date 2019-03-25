let () =
  AesonSpec.sampleGoldenAndServerSpec
    (SubTypeParameter.decodeSubTypeParameter AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (SubTypeParameter.encodeSubTypeParameter Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "subTypeParameter"
    "http://localhost:8081/SubTypeParameter/SubTypeParameter"
    "golden/product/SubTypeParameter";
