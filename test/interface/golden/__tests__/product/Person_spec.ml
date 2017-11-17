let () =
  AesonSpec.sampleGoldenAndServerSpec Person.decodePerson Person.encodePerson "person" "http://localhost:8081/person" "__tests__/golden/product/Person.json";
