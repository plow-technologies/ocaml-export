let () =
  AesonSpec.goldenDirSpec
    SingleSum.decodeSingleSum
    SingleSum.encodeSingleSum
    "singleSum"
    "golden/sum/SingleSum";
