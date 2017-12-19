'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeNameOrIdNumber(x) {
  if (x.tag) {
    return Aeson_encode.object_(/* :: */[
                /* tuple */[
                  "tag",
                  "IdNumber"
                ],
                /* :: */[
                  /* tuple */[
                    "contents",
                    x[0]
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Aeson_encode.object_(/* :: */[
                /* tuple */[
                  "tag",
                  "Name"
                ],
                /* :: */[
                  /* tuple */[
                    "contents",
                    x[0]
                  ],
                  /* [] */0
                ]
              ]);
  }
}

function decodeNameOrIdNumber(json) {
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
      case "IdNumber" : 
          var exit$1 = 0;
          var v;
          try {
            v = Aeson_decode.field("contents", Aeson_decode.$$int, json);
            exit$1 = 2;
          }
          catch (raw_exn$1){
            var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
            if (exn$1[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["IdNumber: " + exn$1[1]]);
            } else {
              throw exn$1;
            }
          }
          if (exit$1 === 2) {
            return /* Ok */Block.__(0, [/* IdNumber */Block.__(1, [v])]);
          }
          break;
      case "Name" : 
          var exit$2 = 0;
          var v$1;
          try {
            v$1 = Aeson_decode.field("contents", Aeson_decode.string, json);
            exit$2 = 2;
          }
          catch (raw_exn$2){
            var exn$2 = Js_exn.internalToOCamlException(raw_exn$2);
            if (exn$2[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["Name: " + exn$2[1]]);
            } else {
              throw exn$2;
            }
          }
          if (exit$2 === 2) {
            return /* Ok */Block.__(0, [/* Name */Block.__(0, [v$1])]);
          }
          break;
      default:
        return /* Error */Block.__(1, ["Unknown tag value found '" + (err + "'.")]);
    }
  }
  
}

exports.encodeNameOrIdNumber = encodeNameOrIdNumber;
exports.decodeNameOrIdNumber = decodeNameOrIdNumber;
/* Aeson_encode Not a pure module */
