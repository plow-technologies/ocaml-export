let () =
  AesonSpec.goldenDirSpec
    (Wrapper.decodeWrapper AesonSpec.decodeIntWithResult)
    (Wrapper.encodeWrapper Aeson.Encode.int)
    "wrapper"
    "golden/product/Wrapper";

  AesonSpec.goldenDirSpec
    Wrapper.decodeIntWrapped
    Wrapper.encodeIntWrapped
    "intWrapped"
    "golden/product/IntWrapped";

  AesonSpec.goldenDirSpec
    Wrapper.decodeMaybeWrapped
    Wrapper.encodeMaybeWrapped
    "maybeWrapped"
    "golden/product/MaybeWrapped";

  AesonSpec.goldenDirSpec
    Wrapper.decodeEitherWrapped
    Wrapper.encodeEitherWrapped
    "eitherWrapped"
    "golden/product/EitherWrapped";

  AesonSpec.goldenDirSpec
    Wrapper.decodeComplexWrapped
    Wrapper.encodeComplexWrapped
    "complexWrapped"
    "golden/product/ComplexWrapped";

  AesonSpec.goldenDirSpec
    Wrapper.decodeSumWrapped
    Wrapper.encodeSumWrapped
    "sumWrapped"
    "golden/product/SumWrapped";

  AesonSpec.goldenDirSpec
    Wrapper.decodeTupleWrapped
    Wrapper.encodeTupleWrapped
    "tupleWrapped"
    "golden/product/TupleWrapped";

  AesonSpec.goldenDirSpec
    (Wrapper.decodeHalfWrapped AesonSpec.decodeIntWithResult)
    (Wrapper.encodeHalfWrapped Aeson.Encode.int)
    "halfWrapped"
    "golden/product/HalfWrapped";

  AesonSpec.goldenDirSpec
    (Wrapper.decodePartiallyWrapped AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Wrapper.encodePartiallyWrapped Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "partiallyWrapped"
    "golden/product/PartiallyWrapped";

  AesonSpec.goldenDirSpec
    (Wrapper.decodeScrambledTypeParameterRefs AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Wrapper.encodeScrambledTypeParameterRefs Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "scrambledTypeParameterRefs"
    "golden/product/ScrambledTypeParameterRefs";

  AesonSpec.goldenDirSpec
    Wrapper.decodeWrappedWrapper
    Wrapper.encodeWrappedWrapper
    "wrappedWrapper"
    "golden/product/WrappedWrapper";

  AesonSpec.goldenDirSpec
    (Wrapper.decodeWrapThree AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Wrapper.encodeWrapThree Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "wrapThree"
    "golden/product/WrapThree";

  AesonSpec.goldenDirSpec
    (Wrapper.decodeWrapThreeUnfilled AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (Wrapper.encodeWrapThreeUnfilled Aeson.Encode.int Aeson.Encode.int Aeson.Encode.int)
    "wrapThreeUnfilled"
    "golden/product/WrapThreeUnfilled";

  AesonSpec.goldenDirSpec
    Wrapper.decodeWrapThreeFilled
    Wrapper.encodeWrapThreeFilled
    "wrapThreeFilled"
    "golden/product/WrapThreeFilled";

  AesonSpec.goldenDirSpec
    (Wrapper.decodeWrapThreePartiallyFilled AesonSpec.decodeIntWithResult)
    (Wrapper.encodeWrapThreePartiallyFilled Aeson.Encode.int)
    "wrapThreePartiallyFilled"
    "golden/product/WrapThreePartiallyFilled";
