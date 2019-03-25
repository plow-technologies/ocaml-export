let () =
  AesonSpec.goldenDirSpec
    UnnamedProduct.decodeUnnamedProduct
    UnnamedProduct.encodeUnnamedProduct
    "unnamedProduct"
    "golden/product/UnnamedProduct";
