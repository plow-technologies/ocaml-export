'use strict';

var AesonSpec        = require("bs-aeson-spec/lib/js/src/AesonSpec.js");
var SubTypeParameter = require("../../product/SubTypeParameter.js");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return SubTypeParameter.decodeSubTypeParameter(AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return SubTypeParameter.encodeSubTypeParameter((function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), param);
      }), "subTypeParameter", "http://localhost:8081/SubTypeParameter/SubTypeParameter", "golden/product/SubTypeParameter");

/*  Not a pure module */
