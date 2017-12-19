'use strict';

var Company   = require("../../product/Company.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(Company.decodeCompany, Company.encodeCompany, "company", "http://localhost:8081/Company/Company", "golden/product/Company");

/*  Not a pure module */
