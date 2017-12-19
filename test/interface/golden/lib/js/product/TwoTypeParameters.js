'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeTwoTypeParameters(encodeA0, encodeA1, x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "ttpId",
                x[/* ttpId */0]
              ],
              /* :: */[
                /* tuple */[
                  "ttpFirst",
                  Curry._1(encodeA0, x[/* ttpFirst */1])
                ],
                /* :: */[
                  /* tuple */[
                    "ttpSecond",
                    Curry._1(encodeA1, x[/* ttpSecond */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodeTwoTypeParameters(decodeA0, decodeA1, json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* ttpId */Aeson_decode.field("ttpId", Aeson_decode.$$int, json),
      /* ttpFirst */Aeson_decode.field("ttpFirst", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA0, a));
            }), json),
      /* ttpSecond */Aeson_decode.field("ttpSecond", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA1, a));
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeTwoTypeParameters: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeTwoTypeParameters = encodeTwoTypeParameters;
exports.decodeTwoTypeParameters = decodeTwoTypeParameters;
/* Aeson_encode Not a pure module */
