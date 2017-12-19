'use strict';

var AesonSpec     = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var SumWithRecord = require("../../sum/SumWithRecord.js");

AesonSpec.sampleGoldenAndServerSpec(SumWithRecord.decodeSumWithRecord, SumWithRecord.encodeSumWithRecord, "sumWithRecord", "http://localhost:8082/SumWithRecord/SumWithRecord", "golden/sum/SumWithRecord");

/*  Not a pure module */
