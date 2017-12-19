'use strict';

var Block   = require("bs-platform/lib/js/block.js");
var Js_json = require("bs-platform/lib/js/js_json.js");

function encodeOnOrOff(x) {
  if (x !== 0) {
    return "Off";
  } else {
    return "On";
  }
}

function decodeOnOrOff(json) {
  var match = Js_json.decodeString(json);
  if (match) {
    var err = match[0];
    switch (err) {
      case "Off" : 
          return /* Ok */Block.__(0, [/* Off */1]);
      case "On" : 
          return /* Ok */Block.__(0, [/* On */0]);
      default:
        return /* Error */Block.__(1, ["decodeOnOrOff: unknown enumeration '" + (err + "'.")]);
    }
  } else {
    return /* Error */Block.__(1, ["decodeOnOrOff: expected a top-level JSON string."]);
  }
}

exports.encodeOnOrOff = encodeOnOrOff;
exports.decodeOnOrOff = decodeOnOrOff;
/* No side effect */
