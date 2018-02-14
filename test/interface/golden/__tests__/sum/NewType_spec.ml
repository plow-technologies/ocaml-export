let () =
  AesonSpec.goldenDirSpec
    NewType.decodeNewType
    NewType.encodeNewType
    "newType"
    "golden/sum/NewType";
