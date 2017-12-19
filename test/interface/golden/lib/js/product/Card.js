'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Js_json      = require("bs-platform/lib/js/js_json.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeSuit(x) {
  switch (x) {
    case 0 : 
        return "Clubs";
    case 1 : 
        return "Diamonds";
    case 2 : 
        return "Hearts";
    case 3 : 
        return "Spades";
    
  }
}

function decodeSuit(json) {
  var match = Js_json.decodeString(json);
  if (match) {
    var err = match[0];
    switch (err) {
      case "Clubs" : 
          return /* Ok */Block.__(0, [/* Clubs */0]);
      case "Diamonds" : 
          return /* Ok */Block.__(0, [/* Diamonds */1]);
      case "Hearts" : 
          return /* Ok */Block.__(0, [/* Hearts */2]);
      case "Spades" : 
          return /* Ok */Block.__(0, [/* Spades */3]);
      default:
        return /* Error */Block.__(1, ["decodeSuit: unknown enumeration '" + (err + "'.")]);
    }
  } else {
    return /* Error */Block.__(1, ["decodeSuit: expected a top-level JSON string."]);
  }
}

function encodeCard(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "cardSuit",
                encodeSuit(x[/* cardSuit */0])
              ],
              /* :: */[
                /* tuple */[
                  "cardValue",
                  x[/* cardValue */1]
                ],
                /* [] */0
              ]
            ]);
}

function decodeCard(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* cardSuit */Aeson_decode.field("cardSuit", (function (a) {
              return Aeson_decode.unwrapResult(decodeSuit(a));
            }), json),
      /* cardValue */Aeson_decode.field("cardValue", Aeson_decode.$$int, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeCard: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeSuit = encodeSuit;
exports.decodeSuit = decodeSuit;
exports.encodeCard = encodeCard;
exports.decodeCard = decodeCard;
/* Aeson_encode Not a pure module */
