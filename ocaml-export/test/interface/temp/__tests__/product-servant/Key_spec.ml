let () =
  AesonSpec.sampleGoldenAndServerSpec
    Key.decodeSqlKey
    Key.encodeSqlKey
    "sqlKey"
    "http://localhost:8081/Key/SqlKey"
    "golden/product/SqlKey";

  AesonSpec.sampleGoldenAndServerSpec
    Key.decodeUserId
    Key.encodeUserId
    "userId"
    "http://localhost:8081/Key/UserId"
    "golden/product/UserId";
