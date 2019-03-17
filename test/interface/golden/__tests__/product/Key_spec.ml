let () =
  AesonSpec.goldenDirSpec
    Key.decodeSqlKey
    Key.encodeSqlKey
    "sqlKey"
    "golden/product/SqlKey";

  AesonSpec.goldenDirSpec
    Key.decodeUserId
    Key.encodeUserId
    "userId"
    "golden/product/UserId";
