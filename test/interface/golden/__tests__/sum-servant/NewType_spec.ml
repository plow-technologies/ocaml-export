let () =
  AesonSpec.sampleGoldenAndServerSpec
    NewType.decodeNewType
    NewType.encodeNewType
    "newType"
    "http://localhost:8082/NewType/NewType"
    "golden/sum/NewType";
