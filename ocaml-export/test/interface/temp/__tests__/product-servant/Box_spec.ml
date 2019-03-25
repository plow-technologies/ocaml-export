let () =
  AesonSpec.sampleGoldenAndServerSpec
    (Box.decodeInnerBox AesonSpec.decodeIntWithResult)
    (Box.encodeInnerBox Aeson.Encode.int)
    "innerBox"
    "http://localhost:8081/Box/InnerBox"
    "golden/product/InnerBox";

  AesonSpec.sampleGoldenAndServerSpec
    Box.decodeOuterBox
    Box.encodeOuterBox
    "outerBox"
    "http://localhost:8081/Box/OuterBox"
    "golden/product/OuterBox";

  AesonSpec.sampleGoldenAndServerSpec
    Box.decodeAuditAction
    Box.encodeAuditAction
    "auditAction"
    "http://localhost:8081/Box/AuditAction"
    "golden/product/AuditAction";

  AesonSpec.sampleGoldenAndServerSpec
    (Box.decodeAuditModel AesonSpec.decodeIntWithResult)
    (Box.encodeAuditModel Aeson.Encode.int)
    "auditModel"
    "http://localhost:8081/Box/AuditModel"
    "golden/product/AuditModel";

  AesonSpec.sampleGoldenAndServerSpec
    Box.decodeUser
    Box.encodeUser
    "user"
    "http://localhost:8081/Box/User"
    "golden/product/User";

  AesonSpec.sampleGoldenAndServerSpec
    Box.decodeUserAudit
    Box.encodeUserAudit
    "userAudit"
    "http://localhost:8081/Box/UserAudit"
    "golden/product/UserAudit";
