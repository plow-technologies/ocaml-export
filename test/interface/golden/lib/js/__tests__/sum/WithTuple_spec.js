'use strict';

var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var WithTuple = require("../../sum/WithTuple.js");

AesonSpec.sampleGoldenAndServerSpec(WithTuple.decodeWithTuple, WithTuple.encodeWithTuple, "withTuple", "http://localhost:8082/WithTuple/WithTuple", "golden/sum/WithTuple");

/*  Not a pure module */
