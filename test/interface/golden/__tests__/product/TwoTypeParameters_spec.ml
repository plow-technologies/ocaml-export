let () =
  AesonSpec.goldenDirSpec
    (TwoTypeParameters.decodeTwoTypeParameters AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (TwoTypeParameters.encodeTwoTypeParameters Aeson.Encode.int Aeson.Encode.int)
    "twoTypeParameters"
    "golden/product/TwoTypeParameters";
