let () =
  AesonSpec.goldenDirSpec
    (ThreeTypeParameters.decodeThree AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (ThreeTypeParameters.encodeThree Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "three"
    "golden/product/Three";
