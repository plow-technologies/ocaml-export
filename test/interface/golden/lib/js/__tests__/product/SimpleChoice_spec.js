'use strict';

var AesonSpec    = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var SimpleChoice = require("../../product/SimpleChoice.js");

AesonSpec.sampleGoldenAndServerSpec(SimpleChoice.decodeSimpleChoice, SimpleChoice.encodeSimpleChoice, "simpleChoice", "http://localhost:8081/SimpleChoice/SimpleChoice", "golden/product/SimpleChoice");

/*  Not a pure module */
