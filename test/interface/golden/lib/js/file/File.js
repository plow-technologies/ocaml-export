'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Curry        = require("bs-platform/lib/js/curry.js");
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
                /* [] */0
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
            }), json)
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

function encodeAutomobile(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "make",
                x[/* make */0]
              ],
              /* :: */[
                /* tuple */[
                  "model",
                  x[/* model */1]
                ],
                /* :: */[
                  /* tuple */[
                    "year",
                    x[/* year */2]
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodeAutomobile(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* make */Aeson_decode.field("make", Aeson_decode.string, json),
      /* model */Aeson_decode.field("model", Aeson_decode.string, json),
      /* year */Aeson_decode.field("year", Aeson_decode.$$int, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeAutomobile: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function encodeBusiness(x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "taxId",
                x[/* taxId */0]
              ],
              /* :: */[
                /* tuple */[
                  "owner",
                  encodePerson(x[/* owner */1])
                ],
                /* :: */[
                  /* tuple */[
                    "employees",
                    Aeson_encode.list(encodePerson, x[/* employees */2])
                  ],
                  /* :: */[
                    /* tuple */[
                      "companyVehicle",
                      Aeson_encode.optional(encodeAutomobile, x[/* companyVehicle */3])
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

function decodeBusiness(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* taxId */Aeson_decode.field("taxId", Aeson_decode.string, json),
      /* owner */Aeson_decode.field("owner", (function (a) {
              return Aeson_decode.unwrapResult(decodePerson(a));
            }), json),
      /* employees */Aeson_decode.field("employees", (function (param) {
              return Aeson_decode.list((function (a) {
                            return Aeson_decode.unwrapResult(decodePerson(a));
                          }), param);
            }), json),
      /* companyVehicle */Aeson_decode.optional((function (param) {
              return Aeson_decode.field("companyVehicle", (function (a) {
                            return Aeson_decode.unwrapResult(decodeAutomobile(a));
                          }), param);
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeBusiness: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function encodeWrapper(a, b, x) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "wrapperA",
                Curry._1(a, x[/* wrapperA */0])
              ],
              /* :: */[
                /* tuple */[
                  "wrapperB",
                  Curry._1(b, x[/* wrapperB */1])
                ],
                /* :: */[
                  /* tuple */[
                    "wrapperC",
                    x[/* wrapperC */2]
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function decodeWrapper(a, b, json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* wrapperA */Aeson_decode.field("wrapperA", (function (x) {
              return Aeson_decode.unwrapResult(Curry._1(a, x));
            }), json),
      /* wrapperB */Aeson_decode.field("wrapperB", (function (x) {
              return Aeson_decode.unwrapResult(Curry._1(b, x));
            }), json),
      /* wrapperC */Aeson_decode.field("wrapperC", Aeson_decode.string, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeWrapper: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

exports.encodePerson     = encodePerson;
exports.decodePerson     = decodePerson;
exports.encodeAutomobile = encodeAutomobile;
exports.decodeAutomobile = decodeAutomobile;
exports.encodeBusiness   = encodeBusiness;
exports.decodeBusiness   = decodeBusiness;
exports.encodeWrapper    = encodeWrapper;
exports.decodeWrapper    = decodeWrapper;
/* Aeson_encode Not a pure module */
