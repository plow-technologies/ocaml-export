'use strict';

var Result    = require("../../sum/Result.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return Result.decodeResult(AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return Result.encodeResult((function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), param);
      }), "result", "http://localhost:8082/Result/Result", "golden/sum/Result");

/*  Not a pure module */
