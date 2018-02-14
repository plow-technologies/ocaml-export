let () =
  AesonSpec.goldenDirSpec
    NameOrIdNumber.decodeNameOrIdNumber
    NameOrIdNumber.encodeNameOrIdNumber
    "nameOrIdNumber"
    "golden/sum/NameOrIdNumber";
