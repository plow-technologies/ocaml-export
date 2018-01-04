let () =
  AesonSpec.sampleGoldenAndServerSpec
    ComplexProduct.decodeComplexProduct
    ComplexProduct.encodeComplexProduct
    "complexProduct"
    "http://localhost:8081/ComplexProduct/ComplexProduct"
    "golden/product/ComplexProduct";
