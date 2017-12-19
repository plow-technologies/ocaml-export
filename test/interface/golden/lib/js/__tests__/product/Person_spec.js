'use strict';

var Person    = require("../../product/Person.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(Person.decodePerson, Person.encodePerson, "person", "http://localhost:8081/Person/Person", "golden/product/Person");

/*  Not a pure module */
