let () =
  AesonSpec.sampleGoldenAndServerSpec
    Company.decodeCompany
    Company.encodeCompany
    "company"
    "http://localhost:8081/Company/Company"
    "golden/product/Company";
