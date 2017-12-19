'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeThree(encodeA0, encodeA1, encodeA2, x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "threeId",
                x[/* threeId */0]
              ],
              /* :: */[
                /* tuple */[
                  "threeFirst",
                  Curry._1(encodeA0, x[/* threeFirst */1])
                ],
                /* :: */[
                  /* tuple */[
                    "threeSecond",
                    Curry._1(encodeA1, x[/* threeSecond */2])
                  ],
                  /* :: */[
                    /* tuple */[
                      "threeThird",
                      Curry._1(encodeA2, x[/* threeThird */3])
                    ],
                    /* :: */[
                      /* tuple */[
                        "threeString",
                        x[/* threeString */4]
                      ],
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]);
}

function decodeThree(decodeA0, decodeA1, decodeA2, json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* threeId */Aeson_decode.field("threeId", Aeson_decode.$$int, json),
      /* threeFirst */Aeson_decode.field("threeFirst", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA0, a));
            }), json),
      /* threeSecond */Aeson_decode.field("threeSecond", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA1, a));
            }), json),
      /* threeThird */Aeson_decode.field("threeThird", (function (a) {
              return Aeson_decode.unwrapResult(Curry._1(decodeA2, a));
            }), json),
      /* threeString */Aeson_decode.field("threeString", Aeson_decode.string, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeThree: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodeThree = encodeThree;
exports.decodeThree = decodeThree;
/* Aeson_encode Not a pure module */
