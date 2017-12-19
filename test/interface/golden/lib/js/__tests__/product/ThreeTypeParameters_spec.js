'use strict';

var AesonSpec           = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var ThreeTypeParameters = require("../../product/ThreeTypeParameters.js");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return ThreeTypeParameters.decodeThree(AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return ThreeTypeParameters.encodeThree((function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), param);
      }), "three", "http://localhost:8081/ThreeTypeParameters/Three", "golden/product/Three");

/*  Not a pure module */
