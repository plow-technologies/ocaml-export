let () =
  AesonSpec.sampleGoldenAndServerSpec
    ComplexProduct.decodeSimple
    ComplexProduct.encodeSimple
    "simple"
    "http://localhost:8081/ComplexProduct/Simple"
    "golden/product/Simple";

  AesonSpec.sampleGoldenAndServerSpec
    ComplexProduct.decodeComplexProduct
    ComplexProduct.encodeComplexProduct
    "complexProduct"
    "http://localhost:8081/ComplexProduct/ComplexProduct"
    "golden/product/ComplexProduct";
