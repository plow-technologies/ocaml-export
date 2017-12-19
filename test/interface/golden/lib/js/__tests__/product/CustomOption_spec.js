'use strict';

var AesonSpec    = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var CustomOption = require("../../product/CustomOption.js");

AesonSpec.sampleGoldenAndServerSpec(CustomOption.decodeCompany2, CustomOption.encodeCompany2, "company2", "http://localhost:8081/CustomOption/Company2", "golden/product/Company2");

/*  Not a pure module */
