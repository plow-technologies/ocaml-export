let () =
  AesonSpec.goldenDirSpec
    (SubTypeParameter.decodeSubTypeParameter AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (SubTypeParameter.encodeSubTypeParameter Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "subTypeParameter"
    "golden/product/SubTypeParameter";
