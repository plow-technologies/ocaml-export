'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeSimpleChoice(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "choice",
                Aeson_encode.either((function (prim) {
                        return prim;
                      }), (function (prim) {
                        return prim;
                      }), x[/* choice */0])
              ],
              /* [] */0
            ]);
}

function decodeSimpleChoice(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[/* choice */Aeson_decode.field("choice", (function (param) {
              return Aeson_decode.either(Aeson_decode.string, Aeson_decode.$$int, param);
            }), json)];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeSimpleChoice: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeSimpleChoice = encodeSimpleChoice;
exports.decodeSimpleChoice = decodeSimpleChoice;
/* Aeson_encode Not a pure module */
