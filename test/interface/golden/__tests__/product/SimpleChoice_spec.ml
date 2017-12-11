let () =
  AesonSpec.sampleGoldenAndServerSpec SimpleChoice.decodeSimpleChoice SimpleChoice.encodeSimpleChoice "simpleChoice" "http://localhost:8081/SimpleChoice/SimpleChoice" "golden/product/SimpleChoice";
