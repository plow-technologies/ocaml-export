'use strict';

var NewType   = require("../../sum/NewType.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(NewType.decodeNewType, NewType.encodeNewType, "newType", "http://localhost:8082/NewType/NewType", "golden/sum/NewType");

/*  Not a pure module */
