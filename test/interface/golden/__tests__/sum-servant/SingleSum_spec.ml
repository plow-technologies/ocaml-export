let () =
  AesonSpec.sampleGoldenAndServerSpec
    SingleSum.decodeSingleSum
    SingleSum.encodeSingleSum
    "nameOrIdNumber"
    "http://localhost:8082/SingleSum/SingleSum"
    "golden/sum/SingleSum";
