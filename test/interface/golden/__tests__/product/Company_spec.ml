let () =
  AesonSpec.sampleGoldenAndServerSpec Company.decodePerson Company.encodePerson "person" "http://localhost:8081/Company/Person" "golden/product/Person.json";

  AesonSpec.sampleGoldenAndServerSpec Company.decodeCompany Company.encodeCompany "company" "http://localhost:8081/Company/Company" "golden/product/Company.json";
