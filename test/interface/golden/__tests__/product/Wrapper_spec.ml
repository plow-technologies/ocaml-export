let () =
  AesonSpec.sampleGoldenAndServerSpec
    (Wrapper.decodeWrapper AesonSpec.decodeIntWithResult)
    (Wrapper.encodeWrapper Aeson.Encode.int)
    "wrapper"
    "http://localhost:8081/Wrapper/Wrapper"
    "golden/product/Wrapper";

  AesonSpec.sampleGoldenAndServerSpec
    Wrapper.decodeIntWrapped
    Wrapper.encodeIntWrapped
    "intWrapped"
    "http://localhost:8081/Wrapper/IntWrapped"
    "golden/product/IntWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    Wrapper.decodeMaybeWrapped
    Wrapper.encodeMaybeWrapped
    "maybeWrapped"
    "http://localhost:8081/Wrapper/MaybeWrapped"
    "golden/product/MaybeWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    Wrapper.decodeEitherWrapped
    Wrapper.encodeEitherWrapped
    "eitherWrapped"
    "http://localhost:8081/Wrapper/EitherWrapped"
    "golden/product/EitherWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    Wrapper.decodeComplexWrapped
    Wrapper.encodeComplexWrapped
    "complexWrapped"
    "http://localhost:8081/Wrapper/ComplexWrapped"
    "golden/product/ComplexWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    Wrapper.decodeSumWrapped
    Wrapper.encodeSumWrapped
    "sumWrapped"
    "http://localhost:8081/Wrapper/SumWrapped"
    "golden/product/SumWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    (Wrapper.decodeHalfWrapped AesonSpec.decodeIntWithResult)
    (Wrapper.encodeHalfWrapped Aeson.Encode.int)
    "halfWrapped"
    "http://localhost:8081/Wrapper/HalfWrapped"
    "golden/product/HalfWrapped";

  AesonSpec.sampleGoldenAndServerSpec
    (Wrapper.decodeScrambledTypeParameterRefs AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Wrapper.encodeScrambledTypeParameterRefs Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "scrambledTypeParameterRefs"
    "http://localhost:8081/Wrapper/ScrambledTypeParameterRefs"
    "golden/product/ScrambledTypeParameterRefs";
