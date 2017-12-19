'use strict';

var OnOrOff   = require("../../sum/OnOrOff.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(OnOrOff.decodeOnOrOff, OnOrOff.encodeOnOrOff, "onOrOff", "http://localhost:8082/OnOrOff/OnOrOff", "golden/sum/OnOrOff");

/*  Not a pure module */
