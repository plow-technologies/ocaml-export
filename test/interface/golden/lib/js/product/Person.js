'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodePerson(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "id",
                x[/* id */0]
              ],
              /* :: */[
                /* tuple */[
                  "name",
                  Aeson_encode.optional((function (prim) {
                          return prim;
                        }), x[/* name */1])
                ],
                /* :: */[
                  /* tuple */[
                    "created",
                    Aeson_encode.date(x[/* created */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodePerson(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* id */Aeson_decode.field("id", Aeson_decode.$$int, json),
      /* name */Aeson_decode.optional((function (param) {
              return Aeson_decode.field("name", Aeson_decode.string, param);
            }), json),
      /* created */Aeson_decode.field("created", Aeson_decode.date, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodePerson: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodePerson = encodePerson;
exports.decodePerson = decodePerson;
/* Aeson_encode Not a pure module */
