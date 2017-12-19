'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Person       = require("./Person.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeCompany(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "address",
                x[/* address */0]
              ],
              /* :: */[
                /* tuple */[
                  "employees",
                  Aeson_encode.list(Person.encodePerson, x[/* employees */1])
                ],
                /* [] */0
              ]
            ]);
}

function decodeCompany(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* address */Aeson_decode.field("address", Aeson_decode.string, json),
      /* employees */Aeson_decode.field("employees", (function (param) {
              return Aeson_decode.list((function (a) {
                            return Aeson_decode.unwrapResult(Person.decodePerson(a));
                          }), param);
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeCompany: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeCompany = encodeCompany;
exports.decodeCompany = decodeCompany;
/* Person Not a pure module */
