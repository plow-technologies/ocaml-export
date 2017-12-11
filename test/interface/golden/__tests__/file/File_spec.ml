let () =
  AesonSpec.sampleGoldenAndServerSpec File.decodePerson File.encodePerson "person" "http://localhost:8083/File/Person" "golden/file/Person";

  AesonSpec.sampleGoldenAndServerSpec File.decodeAutomobile File.encodeAutomobile "automobile" "http://localhost:8083/File/Automobile" "golden/file/Automobile";

  AesonSpec.sampleGoldenAndServerSpec File.decodeBusiness File.encodeBusiness "business" "http://localhost:8083/File/Business" "golden/file/Business";
