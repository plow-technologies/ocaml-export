type 'a0 wrapper =
  { wpa : 'a0
  }

let encodeWrapper encodeA0 x =
  Aeson.Encode.object_
    [ ( "wpa", encodeA0 x.wpa )
    ]

let decodeWrapper decodeA0 json =
  match Aeson.Decode.
    { wpa = field "wpa" (fun a -> unwrapResult (decodeA0 a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapper: " ^ message)

type intWrapped =
  { iw : (int) wrapper
  }

let encodeIntWrapped x =
  Aeson.Encode.object_
    [ ( "iw", (encodeWrapper Aeson.Encode.int) x.iw )
    ]

let decodeIntWrapped json =
  match Aeson.Decode.
    { iw = field "iw" (fun a -> unwrapResult (decodeWrapper (wrapResult int) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeIntWrapped: " ^ message)

type maybeWrapped =
  { mw : (((int) option)) wrapper
  }

let encodeMaybeWrapped x =
  Aeson.Encode.object_
    [ ( "mw", (encodeWrapper (Aeson.Encode.optional Aeson.Encode.int)) x.mw )
    ]

let decodeMaybeWrapped json =
  match Aeson.Decode.
    { mw = field "mw" (fun a -> unwrapResult (decodeWrapper (wrapResult (optional int)) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeMaybeWrapped: " ^ message)

type eitherWrapped =
  { ew : ((int, float) Aeson.Compatibility.Either.t) wrapper
  }

let encodeEitherWrapped x =
  Aeson.Encode.object_
    [ ( "ew", (encodeWrapper (Aeson.Encode.either Aeson.Encode.int Aeson.Encode.float)) x.ew )
    ]

let decodeEitherWrapped json =
  match Aeson.Decode.
    { ew = field "ew" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int Aeson.Decode.float)) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeEitherWrapped: " ^ message)

type complexWrapped =
  { cw : (((string) option, float) Aeson.Compatibility.Either.t) wrapper
  }

let encodeComplexWrapped x =
  Aeson.Encode.object_
    [ ( "cw", (encodeWrapper (Aeson.Encode.either (Aeson.Encode.optional Aeson.Encode.string) Aeson.Encode.float)) x.cw )
    ]

let decodeComplexWrapped json =
  match Aeson.Decode.
    { cw = field "cw" (fun a -> unwrapResult (decodeWrapper (wrapResult (either (optional string) Aeson.Decode.float)) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeComplexWrapped: " ^ message)

type sumWrapped =
  | SW1
  | SW2 of (int) wrapper
  | SW3 of (((string) option)) wrapper
  | SW4 of ((int, string) Aeson.Compatibility.Either.t) wrapper

let encodeSumWrapped x =
  match x with
  | SW1 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW1" )
       ]
  | SW2 y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW2" )
       ; ( "contents", (encodeWrapper Aeson.Encode.int) y0 )
       ]
  | SW3 y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW3" )
       ; ( "contents", (encodeWrapper (Aeson.Encode.optional Aeson.Encode.string)) y0 )
       ]
  | SW4 y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW4" )
       ; ( "contents", (encodeWrapper (Aeson.Encode.either Aeson.Encode.int Aeson.Encode.string)) y0 )
       ]

let decodeSumWrapped json =
  match Aeson.Decode.(field "tag" string json) with
  | "SW1" ->
     Belt.Result.Ok SW1

  | "SW2" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult int) a)) json) with
      | v -> Belt.Result.Ok (SW2 v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("SW2: " ^ message)
     )
  | "SW3" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult (optional string)) a)) json) with
      | v -> Belt.Result.Ok (SW3 v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("SW3: " ^ message)
     )

  | "SW4" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int string)) a)) json) with
      | v -> Belt.Result.Ok (SW4 v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("SW4: " ^ message)
     )
  | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message

type tupleWrapped =
  { tw : ((int * string * float)) wrapper
  }

let encodeTupleWrapped x =
  Aeson.Encode.object_
    [ ( "tw", (encodeWrapper (Aeson.Encode.tuple3 Aeson.Encode.int Aeson.Encode.string Aeson.Encode.float)) x.tw )
    ]

let decodeTupleWrapped json =
  match Aeson.Decode.
    { tw = field "tw" (fun a -> unwrapResult (decodeWrapper (wrapResult (tuple3 int string Aeson.Decode.float)) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeTupleWrapped: " ^ message)

type 'a0 halfWrapped =
  { hw : ((int, 'a0) Aeson.Compatibility.Either.t) wrapper
  }

let encodeHalfWrapped encodeA0 x =
  Aeson.Encode.object_
    [ ( "hw", (encodeWrapper (Aeson.Encode.either Aeson.Encode.int encodeA0)) x.hw )
    ]

let decodeHalfWrapped decodeA0 json =
  match Aeson.Decode.
    { hw = field "hw" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int (fun a -> unwrapResult (decodeA0 a)))) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeHalfWrapped: " ^ message)

type ('a0, 'a1, 'a2) partiallyWrapped =
  { pw : ((int, (string * 'a1 * float * 'a2 * 'a0)) Aeson.Compatibility.Either.t) wrapper
  }

let encodePartiallyWrapped encodeA0 encodeA1 encodeA2 x =
  Aeson.Encode.object_
    [ ( "pw", (encodeWrapper (Aeson.Encode.either Aeson.Encode.int (Aeson.Encode.tuple5 Aeson.Encode.string encodeA1 Aeson.Encode.float encodeA2 encodeA0))) x.pw )
    ]

let decodePartiallyWrapped decodeA0 decodeA1 decodeA2 json =
  match Aeson.Decode.
    { pw = field "pw" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int (tuple5 string (fun a -> unwrapResult (decodeA1 a)) Aeson.Decode.float (fun a -> unwrapResult (decodeA2 a)) (fun a -> unwrapResult (decodeA0 a))))) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePartiallyWrapped: " ^ message)

type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs =
  { stprb : 'a1
  ; stprd : 'a3
  ; stpre : 'a4
  ; stpra : 'a0
  ; stprf : 'a5
  ; stprc : 'a2
  }

let encodeScrambledTypeParameterRefs encodeA0 encodeA1 encodeA2 encodeA3 encodeA4 encodeA5 x =
  Aeson.Encode.object_
    [ ( "stprb", encodeA1 x.stprb )
    ; ( "stprd", encodeA3 x.stprd )
    ; ( "stpre", encodeA4 x.stpre )
    ; ( "stpra", encodeA0 x.stpra )
    ; ( "stprf", encodeA5 x.stprf )
    ; ( "stprc", encodeA2 x.stprc )
    ]

let decodeScrambledTypeParameterRefs decodeA0 decodeA1 decodeA2 decodeA3 decodeA4 decodeA5 json =
  match Aeson.Decode.
    { stprb = field "stprb" (fun a -> unwrapResult (decodeA1 a)) json
    ; stprd = field "stprd" (fun a -> unwrapResult (decodeA3 a)) json
    ; stpre = field "stpre" (fun a -> unwrapResult (decodeA4 a)) json
    ; stpra = field "stpra" (fun a -> unwrapResult (decodeA0 a)) json
    ; stprf = field "stprf" (fun a -> unwrapResult (decodeA5 a)) json
    ; stprc = field "stprc" (fun a -> unwrapResult (decodeA2 a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeScrambledTypeParameterRefs: " ^ message)

type wrappedWrapper =
  { ww : (((int) option) wrapper) option
  }

let encodeWrappedWrapper x =
  Aeson.Encode.object_
    [ ( "ww", (Aeson.Encode.optional (encodeWrapper (Aeson.Encode.optional Aeson.Encode.int))) x.ww )
    ]

let decodeWrappedWrapper json =
  match Aeson.Decode.
    { ww = field "ww" (optional (fun a -> unwrapResult (decodeWrapper (wrapResult (optional int)) a))) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrappedWrapper: " ^ message)

type ('a0, 'a1, 'a2) wrapThree =
  { wp2a : 'a0
  ; wp2b : 'a1
  ; wp2ab : ('a0 * 'a1)
  ; wp2cb : ('a2 * 'a1)
  }

let encodeWrapThree encodeA0 encodeA1 encodeA2 x =
  Aeson.Encode.object_
    [ ( "wp2a", encodeA0 x.wp2a )
    ; ( "wp2b", encodeA1 x.wp2b )
    ; ( "wp2ab", (Aeson.Encode.pair encodeA0 encodeA1) x.wp2ab )
    ; ( "wp2cb", (Aeson.Encode.pair encodeA2 encodeA1) x.wp2cb )
    ]

let decodeWrapThree decodeA0 decodeA1 decodeA2 json =
  match Aeson.Decode.
    { wp2a = field "wp2a" (fun a -> unwrapResult (decodeA0 a)) json
    ; wp2b = field "wp2b" (fun a -> unwrapResult (decodeA1 a)) json
    ; wp2ab = field "wp2ab" (pair (fun a -> unwrapResult (decodeA0 a)) (fun a -> unwrapResult (decodeA1 a))) json
    ; wp2cb = field "wp2cb" (pair (fun a -> unwrapResult (decodeA2 a)) (fun a -> unwrapResult (decodeA1 a))) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapThree: " ^ message)

type ('a0, 'a1, 'a2) wrapThreeUnfilled =
  { zed : string
  ; unfilled : ('a0, 'a1, 'a2) wrapThree
  }

let encodeWrapThreeUnfilled encodeA0 encodeA1 encodeA2 x =
  Aeson.Encode.object_
    [ ( "zed", Aeson.Encode.string x.zed )
    ; ( "unfilled", (encodeWrapThree encodeA0 encodeA1 encodeA2) x.unfilled )
    ]

let decodeWrapThreeUnfilled decodeA0 decodeA1 decodeA2 json =
  match Aeson.Decode.
    { zed = field "zed" string json
    ; unfilled = field "unfilled" (fun a -> unwrapResult (decodeWrapThree decodeA0 decodeA1 decodeA2 a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapThreeUnfilled: " ^ message)

type wrapThreeFilled =
  { foo : string
  ; filled : (int, float, Person.person) wrapThree
  }

let encodeWrapThreeFilled x =
  Aeson.Encode.object_
    [ ( "foo", Aeson.Encode.string x.foo )
    ; ( "filled", (encodeWrapThree Aeson.Encode.int Aeson.Encode.float Person.encodePerson) x.filled )
    ]

let decodeWrapThreeFilled json =
  match Aeson.Decode.
    { foo = field "foo" string json
    ; filled = field "filled" (fun a -> unwrapResult (decodeWrapThree (wrapResult int) (wrapResult Aeson.Decode.float) Person.decodePerson a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapThreeFilled: " ^ message)

type 'a0 wrapThreePartiallyFilled =
  { bar : string
  ; bar2 : (int) list
  ; partiallyFilled : (float, 'a0, float) wrapThree
  }

let encodeWrapThreePartiallyFilled encodeA0 x =
  Aeson.Encode.object_
    [ ( "bar", Aeson.Encode.string x.bar )
    ; ( "bar2", (Aeson.Encode.list Aeson.Encode.int) x.bar2 )
    ; ( "partiallyFilled", (encodeWrapThree Aeson.Encode.float encodeA0 Aeson.Encode.float) x.partiallyFilled )
    ]

let decodeWrapThreePartiallyFilled decodeA0 json =
  match Aeson.Decode.
    { bar = field "bar" string json
    ; bar2 = field "bar2" (list int) json
    ; partiallyFilled = field "partiallyFilled" (fun a -> unwrapResult (decodeWrapThree (wrapResult Aeson.Decode.float) decodeA0 (wrapResult Aeson.Decode.float) a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapThreePartiallyFilled: " ^ message)
