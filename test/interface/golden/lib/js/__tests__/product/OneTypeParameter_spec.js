'use strict';

var AesonSpec        = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var OneTypeParameter = require("../../product/OneTypeParameter.js");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return OneTypeParameter.decodeOneTypeParameter(AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return OneTypeParameter.encodeOneTypeParameter((function (prim) {
                      return prim;
                    }), param);
      }), "oneTypeParameter", "http://localhost:8081/OneTypeParameter/OneTypeParameter", "golden/product/OneTypeParameter");

/*  Not a pure module */
