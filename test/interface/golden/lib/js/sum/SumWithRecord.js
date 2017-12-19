'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Js_json      = require("bs-platform/lib/js/js_json.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeSumWithRecordA1(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "a1",
                x[/* a1 */0]
              ],
              /* [] */0
            ]);
}

function encodeSumWithRecordB2(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "b2",
                x[/* b2 */0]
              ],
              /* :: */[
                /* tuple */[
                  "b3",
                  x[/* b3 */1]
                ],
                /* [] */0
              ]
            ]);
}

function encodeSumWithRecord(x) {
  if (x.tag) {
    var match = Js_json.decodeObject(encodeSumWithRecordB2(x[0]));
    if (match) {
      var dict = match[0];
      dict["tag"] = "B2";
      return dict;
    } else {
      return Aeson_encode.object_(/* [] */0);
    }
  } else {
    var match$1 = Js_json.decodeObject(encodeSumWithRecordA1(x[0]));
    if (match$1) {
      var dict$1 = match$1[0];
      dict$1["tag"] = "A1";
      return dict$1;
    } else {
      return Aeson_encode.object_(/* [] */0);
    }
  }
}

function decodeSumWithRecordA1(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[/* a1 */Aeson_decode.field("a1", Aeson_decode.$$int, json)];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeSumWithRecordA1: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function decodeSumWithRecordB2(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* b2 */Aeson_decode.field("b2", Aeson_decode.string, json),
      /* b3 */Aeson_decode.field("b3", Aeson_decode.$$int, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeSumWithRecordB2: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function decodeSumWithRecord(json) {
  var exit = 0;
  var err;
  try {
    err = Aeson_decode.field("tag", Aeson_decode.string, json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, [exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    switch (err) {
      case "A1" : 
          var match = decodeSumWithRecordA1(json);
          if (match.tag) {
            return /* Error */Block.__(1, ["decodeSumWithRecord: " + match[0]]);
          } else {
            return /* Ok */Block.__(0, [/* A1 */Block.__(0, [match[0]])]);
          }
          break;
      case "B2" : 
          var match$1 = decodeSumWithRecordB2(json);
          if (match$1.tag) {
            return /* Error */Block.__(1, ["decodeSumWithRecord: " + match$1[0]]);
          } else {
            return /* Ok */Block.__(0, [/* B2 */Block.__(1, [match$1[0]])]);
          }
          break;
      default:
        return /* Error */Block.__(1, ["Unknown tag value found '" + (err + "'.")]);
    }
  }
  
}

exports.encodeSumWithRecordA1 = encodeSumWithRecordA1;
exports.encodeSumWithRecordB2 = encodeSumWithRecordB2;
exports.encodeSumWithRecord   = encodeSumWithRecord;
exports.decodeSumWithRecordA1 = decodeSumWithRecordA1;
exports.decodeSumWithRecordB2 = decodeSumWithRecordB2;
exports.decodeSumWithRecord   = decodeSumWithRecord;
/* Aeson_encode Not a pure module */
