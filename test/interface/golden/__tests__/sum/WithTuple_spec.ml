let () =
  AesonSpec.sampleGoldenAndServerSpec WithTuple.decodeWithTuple WithTuple.encodeWithTuple "withTuple" "http://localhost:8082/WithTuple/WithTuple" "golden/sum/WithTuple.json";
