let () =
  AesonSpec.sampleGoldenAndServerSpec
    UnnamedProduct.decodeUnnamedProduct
    UnnamedProduct.encodeUnnamedProduct
    "unnamedProduct"
    "http://localhost:8081/UnnamedProduct/UnnamedProduct"
    "golden/product/UnnamedProduct";
