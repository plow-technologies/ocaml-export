let () =
  AesonSpec.goldenDirSpec
    (Box.decodeInnerBox AesonSpec.decodeIntWithResult)
    (Box.encodeInnerBox Aeson.Encode.int)
    "innerBox"
    "golden/product/InnerBox";

  AesonSpec.goldenDirSpec
    Box.decodeOuterBox
    Box.encodeOuterBox
    "outerBox"
    "golden/product/OuterBox";

  AesonSpec.goldenDirSpec
    Box.decodeAuditAction
    Box.encodeAuditAction
    "auditAction"
    "golden/product/AuditAction";

  AesonSpec.goldenDirSpec
    (Box.decodeAuditModel AesonSpec.decodeIntWithResult)
    (Box.encodeAuditModel Aeson.Encode.int)
    "auditModel"
    "golden/product/AuditModel";

  AesonSpec.goldenDirSpec
    Box.decodeUser
    Box.encodeUser
    "user"
    "golden/product/User";

  AesonSpec.goldenDirSpec
    Box.decodeUserAudit
    Box.encodeUserAudit
    "userAudit"
    "golden/product/UserAudit";
