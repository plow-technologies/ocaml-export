type 'a0 wrapper =
  { wpa : 'a0
  }

val encodeWrapper : ('a0 -> Js_json.t) -> 'a0 wrapper -> Js_json.t

val decodeWrapper : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 wrapper, string) Belt.Result.t

type intWrapped =
  { iw : (int) wrapper
  }

val encodeIntWrapped : intWrapped -> Js_json.t

val decodeIntWrapped : Js_json.t -> (intWrapped, string) Belt.Result.t

type maybeWrapped =
  { mw : ((int) option) wrapper
  }

val encodeMaybeWrapped : maybeWrapped -> Js_json.t

val decodeMaybeWrapped : Js_json.t -> (maybeWrapped, string) Belt.Result.t

type eitherWrapped =
  { ew : ((float, int) Belt.Result.t) wrapper
  }

val encodeEitherWrapped : eitherWrapped -> Js_json.t

val decodeEitherWrapped : Js_json.t -> (eitherWrapped, string) Belt.Result.t

type complexWrapped =
  { cw : ((float, (string) option) Belt.Result.t) wrapper
  }

val encodeComplexWrapped : complexWrapped -> Js_json.t

val decodeComplexWrapped : Js_json.t -> (complexWrapped, string) Belt.Result.t

type sumWrapped =
  | SW1
  | SW2 of (int) wrapper
  | SW3 of ((string) option) wrapper
  | SW4 of ((string, int) Belt.Result.t) wrapper

val encodeSumWrapped : sumWrapped -> Js_json.t

val decodeSumWrapped : Js_json.t -> (sumWrapped, string) Belt.Result.t

type tupleWrapped =
  { tw : ((int * string * float)) wrapper
  }

val encodeTupleWrapped : tupleWrapped -> Js_json.t

val decodeTupleWrapped : Js_json.t -> (tupleWrapped, string) Belt.Result.t

type 'a0 halfWrapped =
  { hw : ((int, 'a0) Belt.Result.t) wrapper
  }

val encodeHalfWrapped : ('a0 -> Js_json.t) -> 'a0 halfWrapped -> Js_json.t

val decodeHalfWrapped : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 halfWrapped, string) Belt.Result.t

type ('a0, 'a1, 'a2) partiallyWrapped =
  { pw : (((string * 'a1 * float * 'a2 * 'a0), int) Belt.Result.t) wrapper
  }

val encodePartiallyWrapped : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) partiallyWrapped -> Js_json.t

val decodePartiallyWrapped : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> (Js_json.t -> ('a2, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1, 'a2) partiallyWrapped, string) Belt.Result.t

type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs =
  { stprb : 'a1
  ; stprd : 'a3
  ; stpre : 'a4
  ; stpra : 'a0
  ; stprf : 'a5
  ; stprc : 'a2
  }

val encodeScrambledTypeParameterRefs : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a3 -> Js_json.t) -> ('a4 -> Js_json.t) -> ('a5 -> Js_json.t) -> ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs -> Js_json.t

val decodeScrambledTypeParameterRefs : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> (Js_json.t -> ('a2, string) Belt.Result.t) -> (Js_json.t -> ('a3, string) Belt.Result.t) -> (Js_json.t -> ('a4, string) Belt.Result.t) -> (Js_json.t -> ('a5, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs, string) Belt.Result.t

type wrappedWrapper =
  { ww : (((int) option) wrapper) option
  }

val encodeWrappedWrapper : wrappedWrapper -> Js_json.t

val decodeWrappedWrapper : Js_json.t -> (wrappedWrapper, string) Belt.Result.t

type ('a0, 'a1, 'a2) wrapThree =
  { wp2a : 'a0
  ; wp2b : 'a1
  ; wp2ab : ('a0 * 'a1)
  ; wp2cb : ('a2 * 'a1)
  }

val encodeWrapThree : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) wrapThree -> Js_json.t

val decodeWrapThree : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> (Js_json.t -> ('a2, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1, 'a2) wrapThree, string) Belt.Result.t

type ('a0, 'a1, 'a2) wrapThreeUnfilled =
  { zed : string
  ; unfilled : ('a0, 'a1, 'a2) wrapThree
  }

val encodeWrapThreeUnfilled : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) wrapThreeUnfilled -> Js_json.t

val decodeWrapThreeUnfilled : (Js_json.t -> ('a0, string) Belt.Result.t) -> (Js_json.t -> ('a1, string) Belt.Result.t) -> (Js_json.t -> ('a2, string) Belt.Result.t) -> Js_json.t -> (('a0, 'a1, 'a2) wrapThreeUnfilled, string) Belt.Result.t

type wrapThreeFilled =
  { foo : string
  ; filled : (int, float, Person.person) wrapThree
  }

val encodeWrapThreeFilled : wrapThreeFilled -> Js_json.t

val decodeWrapThreeFilled : Js_json.t -> (wrapThreeFilled, string) Belt.Result.t

type 'a0 wrapThreePartiallyFilled =
  { bar : string
  ; bar2 : (int) list
  ; partiallyFilled : (float, 'a0, float) wrapThree
  }

val encodeWrapThreePartiallyFilled : ('a0 -> Js_json.t) -> 'a0 wrapThreePartiallyFilled -> Js_json.t

val decodeWrapThreePartiallyFilled : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 wrapThreePartiallyFilled, string) Belt.Result.t
