let () =
  AesonSpec.sampleGoldenAndServerSpec
    Person.decodePerson
    Person.encodePerson
    "person"
    "http://localhost:8081/Person/Person"
    "golden/product/Person";
