let () =
  AesonSpec.goldenDirSpec
    SumVariant.decodeSumVariant
    SumVariant.encodeSumVariant
    "sumVariant"
    "golden/sum/SumVariant";
