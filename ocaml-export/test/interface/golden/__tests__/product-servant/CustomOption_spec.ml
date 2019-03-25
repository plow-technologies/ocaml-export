let () =
  AesonSpec.sampleGoldenAndServerSpec CustomOption.decodeCompany2 CustomOption.encodeCompany2 "company2" "http://localhost:8081/CustomOption/Company2" "golden/product/Company2";
