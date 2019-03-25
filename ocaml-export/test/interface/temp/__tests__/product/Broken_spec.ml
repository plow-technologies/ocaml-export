let () =
  AesonSpec.goldenDirSpec
    (Broken.decodeInnerBox AesonSpec.decodeIntWithResult)
    (Broken.encodeInnerBox Aeson.Encode.int)
    "innerBox"
    "golden/product/InnerBox";

  AesonSpec.goldenDirSpec
    Broken.decodeOuterBox
    Broken.encodeOuterBox
    "outerBox"
    "golden/product/OuterBox";
