let () =
  AesonSpec.sampleGoldenAndServerSpec CustomOption.decodePerson CustomOption.encodePerson "person" "http://localhost:8081/CustomOption/Person" "golden/product/Person";

  AesonSpec.sampleGoldenAndServerSpec CustomOption.decodeCompany2 CustomOption.encodeCompany2 "company2" "http://localhost:8081/CustomOption/Company2" "golden/product/Company2";
