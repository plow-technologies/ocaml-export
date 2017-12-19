'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Person       = require("./Person.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeCompany2(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "address2",
                x[/* address2 */0]
              ],
              /* :: */[
                /* tuple */[
                  "boss",
                  Aeson_encode.optional(Person.encodePerson, x[/* boss */1])
                ],
                /* [] */0
              ]
            ]);
}

function decodeCompany2(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* address2 */Aeson_decode.field("address2", Aeson_decode.string, json),
      /* boss */Aeson_decode.optional((function (param) {
              return Aeson_decode.field("boss", (function (a) {
                            return Aeson_decode.unwrapResult(Person.decodePerson(a));
                          }), param);
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeCompany2: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeCompany2 = encodeCompany2;
exports.decodeCompany2 = decodeCompany2;
/* Person Not a pure module */
