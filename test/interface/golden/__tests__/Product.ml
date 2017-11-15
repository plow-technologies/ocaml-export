let () =
  (*  AesonSpec.sampleGoldenAndServerSpec Person.decodePerson Person.encodePerson "person" "http://localhost:8081/person" "__tests__/golden/Person.json"; *)
  AesonSpec.sampleGoldenAndServerSpec Card.decodeCard Card.encodeCard "card" "http://localhost:8081/card" "__tests__/golden/Card.json";
