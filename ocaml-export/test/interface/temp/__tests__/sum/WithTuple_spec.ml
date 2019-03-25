let () =
  AesonSpec.goldenDirSpec
    WithTuple.decodeWithTuple
    WithTuple.encodeWithTuple
    "withTuple"
    "golden/sum/WithTuple";
