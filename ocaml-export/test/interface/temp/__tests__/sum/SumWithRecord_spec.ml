let () =
  AesonSpec.goldenDirSpec
    SumWithRecord.decodeSumWithRecord
    SumWithRecord.encodeSumWithRecord
    "sumWithRecord"
    "golden/sum/SumWithRecord";
