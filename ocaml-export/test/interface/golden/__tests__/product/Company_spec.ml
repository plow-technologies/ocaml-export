let () =
  AesonSpec.goldenDirSpec
    Company.decodeCompany
    Company.encodeCompany
    "company"
    "golden/product/Company";
