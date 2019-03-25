let () =
  AesonSpec.goldenDirSpec
    CustomOption.decodeCompany2
    CustomOption.encodeCompany2
    "company2"
    "golden/product/Company2";
