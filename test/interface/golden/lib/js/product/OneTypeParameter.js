'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeOneTypeParameter(encodeA0, x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "otpId",
                x[/* otpId */0]
              ],
              /* :: */[
                /* tuple */[
                  "otpFirst",
                  Curry._1(encodeA0, x[/* otpFirst */1])
                ],
                /* [] */0
              ]
            ]);
}

function decodeOneTypeParameter(decodeA0, json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* otpId */Aeson_decode.field("otpId", Aeson_decode.$$int, json),
      /* otpFirst */Aeson_decode.field("otpFirst", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA0, a));
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeOneTypeParameter: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeOneTypeParameter = encodeOneTypeParameter;
exports.decodeOneTypeParameter = decodeOneTypeParameter;
/* Aeson_encode Not a pure module */
