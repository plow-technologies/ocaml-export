let () =
  AesonSpec.sampleGoldenAndServerSpec Company.decodePerson Company.encodePerson "person" "http://localhost:8081/person" "__tests__/golden/Person.json";

  AesonSpec.sampleGoldenAndServerSpec Company.decodeCompany Company.encodeCompany "company" "http://localhost:8081/company" "__tests__/golden/Company.json";
