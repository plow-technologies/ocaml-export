let () =
  AesonSpec.sampleGoldenAndServerSpec NameOrIdNumber.decodeNameOrIdNumber NameOrIdNumber.encodeNameOrIdNumber "nameOrIdNumber" "http://localhost:8082/NameOrIdNumber/NameOrIdNumber" "golden/sum/NameOrIdNumber.json";
