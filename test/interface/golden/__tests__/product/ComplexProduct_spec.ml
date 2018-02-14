let () =
  AesonSpec.goldenDirSpec
    ComplexProduct.decodeSimple
    ComplexProduct.encodeSimple
    "simple"
    "golden/product/Simple";

  AesonSpec.goldenDirSpec
    ComplexProduct.decodeComplexProduct
    ComplexProduct.encodeComplexProduct
    "complexProduct"
    "golden/product/ComplexProduct";
