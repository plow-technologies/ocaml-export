'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeWithTuple(x) {
  return Aeson_encode.pair((function (prim) {
                return prim;
              }), (function (prim) {
                return prim;
              }), x[0]);
}

function decodeWithTuple(json) {
  var exit = 0;
  var v;
  try {
    v = Aeson_decode.pair(Aeson_decode.$$int, Aeson_decode.$$int, json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeWithTuple: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [/* WithTuple */[v]]);
  }
  
}

exports.encodeWithTuple = encodeWithTuple;
exports.decodeWithTuple = decodeWithTuple;
/* Aeson_encode Not a pure module */
