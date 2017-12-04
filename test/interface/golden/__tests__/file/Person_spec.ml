let () =
  AesonSpec.sampleGoldenAndServerSpec File.decodePerson File.encodePerson "person" "http://localhost:8083/File/Person" "golden/file/Person.json";
