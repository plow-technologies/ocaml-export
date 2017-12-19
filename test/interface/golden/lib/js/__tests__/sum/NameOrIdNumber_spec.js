'use strict';

var AesonSpec      = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var NameOrIdNumber = require("../../sum/NameOrIdNumber.js");

AesonSpec.sampleGoldenAndServerSpec(NameOrIdNumber.decodeNameOrIdNumber, NameOrIdNumber.encodeNameOrIdNumber, "nameOrIdNumber", "http://localhost:8082/NameOrIdNumber/NameOrIdNumber", "golden/sum/NameOrIdNumber");

/*  Not a pure module */
