'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeSubTypeParameter(encodeA0, encodeA1, encodeA2, x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "listA",
                Aeson_encode.list(encodeA0, x[/* listA */0])
              ],
              /* :: */[
                /* tuple */[
                  "maybeB",
                  Aeson_encode.optional(encodeA1, x[/* maybeB */1])
                ],
                /* :: */[
                  /* tuple */[
                    "tupleC",
                    Aeson_encode.pair(encodeA2, encodeA1, x[/* tupleC */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodeSubTypeParameter(decodeA0, decodeA1, decodeA2, json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* listA */Aeson_decode.field("listA", (function (param) {
              return Aeson_decode.list((function (a) {
                            return Aeson_decode.unwrapResult(Curry._1(decodeA0, a));
                          }), param);
            }), json),
      /* maybeB */Aeson_decode.optional((function (param) {
              return Aeson_decode.field("maybeB", (function (a) {
                            return Aeson_decode.unwrapResult(Curry._1(decodeA1, a));
                          }), param);
            }), json),
      /* tupleC */Aeson_decode.field("tupleC", (function (param) {
              return Aeson_decode.pair((function (a) {
                            return Aeson_decode.unwrapResult(Curry._1(decodeA2, a));
                          }), (function (a) {
                            return Aeson_decode.unwrapResult(Curry._1(decodeA1, a));
                          }), param);
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeSubTypeParameter: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeSubTypeParameter = encodeSubTypeParameter;
exports.decodeSubTypeParameter = decodeSubTypeParameter;
/* Aeson_encode Not a pure module */
