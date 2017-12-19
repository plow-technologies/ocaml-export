'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeResult(encodeA0, encodeA1, x) {
  if (x.tag) {
    return Aeson_encode.object_(/* :: */[
                /* tuple */[
                  "tag",
                  "Error"
                ],
                /* :: */[
                  /* tuple */[
                    "contents",
                    Curry._1(encodeA1, x[0])
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Aeson_encode.object_(/* :: */[
                /* tuple */[
                  "tag",
                  "Success"
                ],
                /* :: */[
                  /* tuple */[
                    "contents",
                    Curry._1(encodeA0, x[0])
                  ],
                  /* [] */0
                ]
              ]);
  }
}

function decodeResult(decodeA0, decodeA1, json) {
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
      case "Error" : 
          var exit$1 = 0;
          var v;
          try {
            v = Aeson_decode.field("contents", (function (a) {
                    return Aeson_decode.unwrapResult(Curry._1(decodeA1, a));
                  }), json);
            exit$1 = 2;
          }
          catch (raw_exn$1){
            var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
            if (exn$1[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["Error: " + exn$1[1]]);
            } else {
              throw exn$1;
            }
          }
          if (exit$1 === 2) {
            return /* Ok */Block.__(0, [/* Error */Block.__(1, [v])]);
          }
          break;
      case "Success" : 
          var exit$2 = 0;
          var v$1;
          try {
            v$1 = Aeson_decode.field("contents", (function (a) {
                    return Aeson_decode.unwrapResult(Curry._1(decodeA0, a));
                  }), json);
            exit$2 = 2;
          }
          catch (raw_exn$2){
            var exn$2 = Js_exn.internalToOCamlException(raw_exn$2);
            if (exn$2[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["Success: " + exn$2[1]]);
            } else {
              throw exn$2;
            }
          }
          if (exit$2 === 2) {
            return /* Ok */Block.__(0, [/* Success */Block.__(0, [v$1])]);
          }
          break;
      default:
        return /* Error */Block.__(1, ["Unknown tag value found '" + (err + "'.")]);
    }
  }
  
}

exports.encodeResult = encodeResult;
exports.decodeResult = decodeResult;
/* Aeson_encode Not a pure module */
