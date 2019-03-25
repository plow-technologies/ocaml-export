let () =
  AesonSpec.goldenDirSpec
    SimpleChoice.decodeSimpleChoice
    SimpleChoice.encodeSimpleChoice
    "simpleChoice"
    "golden/product/SimpleChoice";
