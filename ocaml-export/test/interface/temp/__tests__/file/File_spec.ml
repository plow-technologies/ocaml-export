let () =
  AesonSpec.goldenDirSpec
    File.decodePerson
    File.encodePerson
    "person"
    "golden/file/Person";

  AesonSpec.goldenDirSpec
    File.decodeAutomobile
    File.encodeAutomobile
    "automobile"
    "golden/file/Automobile";

  AesonSpec.goldenDirSpec
    File.decodeBusiness
    File.encodeBusiness
    "business"
    "golden/file/Business";

  AesonSpec.goldenDirSpec
    (File.decodeWrapper AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (File.encodeWrapper Aeson.Encode.int Aeson.Encode.int)
    "wrapper"
    "golden/file/Wrapper";

  AesonSpec.goldenDirSpec
    File.decodeAutoDependingOnManual
    File.encodeAutoDependingOnManual
    "autoDependingOnManual"
    "golden/file/AutoDependingOnManual";

  AesonSpec.goldenDirSpec
    File.decodeNonGenericType
    File.encodeNonGenericType
    "nonGenericType"
    "golden/file/NonGenericType";
