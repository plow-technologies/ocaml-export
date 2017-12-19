'use strict';

var AesonSpec  = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var SumVariant = require("../../sum/SumVariant.js");

AesonSpec.sampleGoldenAndServerSpec(SumVariant.decodeSumVariant, SumVariant.encodeSumVariant, "sumVariant", "http://localhost:8082/SumVariant/SumVariant", "golden/sum/SumVariant");

/*  Not a pure module */
