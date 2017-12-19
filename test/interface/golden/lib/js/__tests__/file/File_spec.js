'use strict';

var File      = require("../../file/File.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(File.decodePerson, File.encodePerson, "person", "http://localhost:8083/File/Person", "golden/file/Person");

AesonSpec.sampleGoldenAndServerSpec(File.decodeAutomobile, File.encodeAutomobile, "automobile", "http://localhost:8083/File/Automobile", "golden/file/Automobile");

AesonSpec.sampleGoldenAndServerSpec(File.decodeBusiness, File.encodeBusiness, "business", "http://localhost:8083/File/Business", "golden/file/Business");

AesonSpec.sampleGoldenAndServerSpec((function (param) {
        return File.decodeWrapper(AesonSpec.decodeIntWithResult, AesonSpec.decodeIntWithResult, param);
      }), (function (param) {
        return File.encodeWrapper((function (prim) {
                      return prim;
                    }), (function (prim) {
                      return prim;
                    }), param);
      }), "wrapper", "http://localhost:8083/File/Wrapper", "golden/file/Wrapper");

/*  Not a pure module */
