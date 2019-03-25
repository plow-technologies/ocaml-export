let () =
  AesonSpec.goldenDirSpec
    OnOrOff.decodeOnOrOff
    OnOrOff.encodeOnOrOff
    "onOrOff"
    "golden/sum/OnOrOff";
