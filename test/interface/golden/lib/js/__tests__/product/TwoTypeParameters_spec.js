'use strict';

var AesonSpec         = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var TwoTypeParameters = require("../../product/TwoTypeParameters.js");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return TwoTypeParameters.decodeTwoTypeParameters(AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return TwoTypeParameters.encodeTwoTypeParameters((function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), param);
      }), "twoTypeParameters", "http://localhost:8081/TwoTypeParameters/TwoTypeParameters", "golden/product/TwoTypeParameters");

/*  Not a pure module */
