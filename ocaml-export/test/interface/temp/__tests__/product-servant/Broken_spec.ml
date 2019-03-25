let () =
  AesonSpec.sampleGoldenAndServerSpec
    (Broken.decodeInnerBox AesonSpec.decodeIntWithResult)
    (Broken.encodeInnerBox Aeson.Encode.int)
    "innerBox"
    "http://localhost:8081/Broken/InnerBox"
    "golden/product/InnerBox";

  AesonSpec.sampleGoldenAndServerSpec
    Broken.decodeOuterBox
    Broken.encodeOuterBox
    "outerBox"
    "http://localhost:8081/Broken/OuterBox"
    "golden/product/OuterBox";
