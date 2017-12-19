'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var Js_json      = require("bs-platform/lib/js/js_json.js");
var Caml_array   = require("bs-platform/lib/js/caml_array.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodeSumVariant(x) {
  if (typeof x === "number") {
    return Aeson_encode.object_(/* :: */[
                /* tuple */[
                  "tag",
                  "HasNothing"
                ],
                /* [] */0
              ]);
  } else {
    switch (x.tag | 0) {
      case 0 : 
          return Aeson_encode.object_(/* :: */[
                      /* tuple */[
                        "tag",
                        "HasSingleInt"
                      ],
                      /* :: */[
                        /* tuple */[
                          "contents",
                          x[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 1 : 
          return Aeson_encode.object_(/* :: */[
                      /* tuple */[
                        "tag",
                        "HasSingleTuple"
                      ],
                      /* :: */[
                        /* tuple */[
                          "contents",
                          Aeson_encode.pair((function (prim) {
                                  return prim;
                                }), (function (prim) {
                                  return prim;
                                }), x[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 2 : 
          return Aeson_encode.object_(/* :: */[
                      /* tuple */[
                        "tag",
                        "HasMultipleInts"
                      ],
                      /* :: */[
                        /* tuple */[
                          "contents",
                          /* array */[
                            x[0],
                            x[1]
                          ]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 3 : 
          return Aeson_encode.object_(/* :: */[
                      /* tuple */[
                        "tag",
                        "HasMultipleTuples"
                      ],
                      /* :: */[
                        /* tuple */[
                          "contents",
                          /* array */[
                            Aeson_encode.pair((function (prim) {
                                    return prim;
                                  }), (function (prim) {
                                    return prim;
                                  }), x[0]),
                            Aeson_encode.pair((function (prim) {
                                    return prim;
                                  }), (function (prim) {
                                    return prim;
                                  }), x[1])
                          ]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 4 : 
          return Aeson_encode.object_(/* :: */[
                      /* tuple */[
                        "tag",
                        "HasMixed"
                      ],
                      /* :: */[
                        /* tuple */[
                          "contents",
                          /* array */[
                            x[0],
                            x[1],
                            x[2]
                          ]
                        ],
                        /* [] */0
                      ]
                    ]);
      
    }
  }
}

function decodeSumVariant(json) {
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
      case "HasMixed" : 
          var match = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match) {
            var v = match[0];
            var exit$1 = 0;
            var v0;
            try {
              v0 = Aeson_decode.$$int(Caml_array.caml_array_get(v, 0));
              exit$1 = 2;
            }
            catch (raw_exn$1){
              var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
              if (exn$1[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["HasMixed: " + exn$1[1]]);
              } else {
                throw exn$1;
              }
            }
            if (exit$1 === 2) {
              var exit$2 = 0;
              var v1;
              try {
                v1 = Aeson_decode.string(Caml_array.caml_array_get(v, 1));
                exit$2 = 3;
              }
              catch (raw_exn$2){
                var exn$2 = Js_exn.internalToOCamlException(raw_exn$2);
                if (exn$2[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["HasMixed: " + exn$2[1]]);
                } else {
                  throw exn$2;
                }
              }
              if (exit$2 === 3) {
                var exit$3 = 0;
                var v2;
                try {
                  v2 = Aeson_decode.$$float(Caml_array.caml_array_get(v, 2));
                  exit$3 = 4;
                }
                catch (raw_exn$3){
                  var exn$3 = Js_exn.internalToOCamlException(raw_exn$3);
                  if (exn$3[0] === Aeson_decode.DecodeError) {
                    return /* Error */Block.__(1, ["HasMixed: " + exn$3[1]]);
                  } else {
                    throw exn$3;
                  }
                }
                if (exit$3 === 4) {
                  return /* Ok */Block.__(0, [/* HasMixed */Block.__(4, [
                                v0,
                                v1,
                                v2
                              ])]);
                }
                
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["HasMixed expected an array."]);
          }
          break;
      case "HasMultipleInts" : 
          var match$1 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match$1) {
            var v$1 = match$1[0];
            var exit$4 = 0;
            var v0$1;
            try {
              v0$1 = Aeson_decode.$$int(Caml_array.caml_array_get(v$1, 0));
              exit$4 = 2;
            }
            catch (raw_exn$4){
              var exn$4 = Js_exn.internalToOCamlException(raw_exn$4);
              if (exn$4[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["HasMultipleInts: " + exn$4[1]]);
              } else {
                throw exn$4;
              }
            }
            if (exit$4 === 2) {
              var exit$5 = 0;
              var v1$1;
              try {
                v1$1 = Aeson_decode.$$int(Caml_array.caml_array_get(v$1, 1));
                exit$5 = 3;
              }
              catch (raw_exn$5){
                var exn$5 = Js_exn.internalToOCamlException(raw_exn$5);
                if (exn$5[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["HasMultipleInts: " + exn$5[1]]);
                } else {
                  throw exn$5;
                }
              }
              if (exit$5 === 3) {
                return /* Ok */Block.__(0, [/* HasMultipleInts */Block.__(2, [
                              v0$1,
                              v1$1
                            ])]);
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["HasMultipleInts expected an array."]);
          }
          break;
      case "HasMultipleTuples" : 
          var match$2 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match$2) {
            var v$2 = match$2[0];
            var exit$6 = 0;
            var v0$2;
            try {
              v0$2 = Aeson_decode.pair(Aeson_decode.$$int, Aeson_decode.$$int, Caml_array.caml_array_get(v$2, 0));
              exit$6 = 2;
            }
            catch (raw_exn$6){
              var exn$6 = Js_exn.internalToOCamlException(raw_exn$6);
              if (exn$6[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["HasMultipleTuples: " + exn$6[1]]);
              } else {
                throw exn$6;
              }
            }
            if (exit$6 === 2) {
              var exit$7 = 0;
              var v1$2;
              try {
                v1$2 = Aeson_decode.pair(Aeson_decode.$$int, Aeson_decode.$$int, Caml_array.caml_array_get(v$2, 1));
                exit$7 = 3;
              }
              catch (raw_exn$7){
                var exn$7 = Js_exn.internalToOCamlException(raw_exn$7);
                if (exn$7[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["HasMultipleTuples: " + exn$7[1]]);
                } else {
                  throw exn$7;
                }
              }
              if (exit$7 === 3) {
                return /* Ok */Block.__(0, [/* HasMultipleTuples */Block.__(3, [
                              v0$2,
                              v1$2
                            ])]);
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["HasMultipleTuples expected an array."]);
          }
          break;
      case "HasNothing" : 
          return /* Ok */Block.__(0, [/* HasNothing */0]);
      case "HasSingleInt" : 
          var exit$8 = 0;
          var v$3;
          try {
            v$3 = Aeson_decode.field("contents", Aeson_decode.$$int, json);
            exit$8 = 2;
          }
          catch (raw_exn$8){
            var exn$8 = Js_exn.internalToOCamlException(raw_exn$8);
            if (exn$8[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["HasSingleInt: " + exn$8[1]]);
            } else {
              throw exn$8;
            }
          }
          if (exit$8 === 2) {
            return /* Ok */Block.__(0, [/* HasSingleInt */Block.__(0, [v$3])]);
          }
          break;
      case "HasSingleTuple" : 
          var exit$9 = 0;
          var v$4;
          try {
            v$4 = Aeson_decode.field("contents", (function (param) {
                    return Aeson_decode.pair(Aeson_decode.$$int, Aeson_decode.$$int, param);
                  }), json);
            exit$9 = 2;
          }
          catch (raw_exn$9){
            var exn$9 = Js_exn.internalToOCamlException(raw_exn$9);
            if (exn$9[0] === Aeson_decode.DecodeError) {
              return /* Error */Block.__(1, ["HasSingleTuple: " + exn$9[1]]);
            } else {
              throw exn$9;
            }
          }
          if (exit$9 === 2) {
            return /* Ok */Block.__(0, [/* HasSingleTuple */Block.__(1, [v$4])]);
          }
          break;
      default:
        return /* Error */Block.__(1, ["Unknown tag value found '" + (err + "'.")]);
    }
  }
  
}

exports.encodeSumVariant = encodeSumVariant;
exports.decodeSumVariant = decodeSumVariant;
/* Aeson_encode Not a pure module */
