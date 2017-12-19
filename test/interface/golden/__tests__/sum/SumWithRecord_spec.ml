let () =
  AesonSpec.sampleGoldenAndServerSpec
    SumWithRecord.decodeSumWithRecord
    SumWithRecord.encodeSumWithRecord
    "sumWithRecord"
    "http://localhost:8082/SumWithRecord/SumWithRecord"
    "golden/sum/SumWithRecord";
