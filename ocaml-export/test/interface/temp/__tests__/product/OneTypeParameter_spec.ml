let () =
  AesonSpec.goldenDirSpec
    (OneTypeParameter.decodeOneTypeParameter AesonSpec.decodeIntWithResult)
    (OneTypeParameter.encodeOneTypeParameter Aeson.Encode.int)
    "oneTypeParameter"
    "golden/product/OneTypeParameter";
