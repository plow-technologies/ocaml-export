'use strict';

var Card      = require("../../product/Card.js");
var AesonSpec = require("bs-aeson-spec/lib/js/src/AesonSpec.js");

AesonSpec.sampleGoldenAndServerSpec(Card.decodeSuit, Card.encodeSuit, "suit", "http://localhost:8081/Card/Suit", "golden/product/Suit");

AesonSpec.sampleGoldenAndServerSpec(Card.decodeCard, Card.encodeCard, "card", "http://localhost:8081/Card/Card", "golden/product/Card");

/*  Not a pure module */
