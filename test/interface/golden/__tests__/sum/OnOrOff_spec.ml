let () =
  AesonSpec.sampleGoldenAndServerSpec OnOrOff.decodeOnOrOff OnOrOff.encodeOnOrOff "onOrOff" "http://localhost:8082/OnOrOff/OnOrOff" "golden/sum/OnOrOff.json";
