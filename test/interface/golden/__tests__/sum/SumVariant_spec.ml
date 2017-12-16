let () =
  AesonSpec.sampleGoldenAndServerSpec SumVariant.decodeSumVariant SumVariant.encodeSumVariant "sumVariant" "http://localhost:8082/SumVariant/SumVariant" "golden/sum/SumVariant";
