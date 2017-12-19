'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");

function encodeNewType(x) {
  return x[0];
}

function decodeNewType(json) {
  var exit = 0;
  var v;
  try {
    v = Aeson_decode.$$int(json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeNewType: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [/* NewType */[v]]);
  }
  
}

exports.encodeNewType = encodeNewType;
exports.decodeNewType = decodeNewType;
/* No side effect */
