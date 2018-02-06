type 'a0 wrapper =
  { wpa : 'a0
  }

val encodeWrapper : ('a0 -> Js_json.t) -> 'a0 wrapper -> Js_json.t

val decodeWrapper : (Js_json.t -> ('a0, string) Js_result.t) -> Js_json.t -> ('a0 wrapper, string) Js_result.t

type intWrapped =
  { iw : (int) wrapper
  }

val encodeIntWrapped : intWrapped -> Js_json.t

val decodeIntWrapped : Js_json.t -> (intWrapped, string) Js_result.t

type maybeWrapped =
  { mw : (((int) option)) wrapper
  }

val encodeMaybeWrapped : maybeWrapped -> Js_json.t

val decodeMaybeWrapped : Js_json.t -> (maybeWrapped, string) Js_result.t

type eitherWrapped =
  { ew : ((int, float) Aeson.Compatibility.Either.t) wrapper
  }

val encodeEitherWrapped : eitherWrapped -> Js_json.t

val decodeEitherWrapped : Js_json.t -> (eitherWrapped, string) Js_result.t

type complexWrapped =
  { cw : (((string) option, float) Aeson.Compatibility.Either.t) wrapper
  }

val encodeComplexWrapped : complexWrapped -> Js_json.t

val decodeComplexWrapped : Js_json.t -> (complexWrapped, string) Js_result.t

type sumWrapped =
  | SW1
  | SW2 of (int) wrapper
  | SW3 of (((string) option)) wrapper
  | SW4 of ((int, string) Aeson.Compatibility.Either.t) wrapper

val encodeSumWrapped : sumWrapped -> Js_json.t

val decodeSumWrapped : Js_json.t -> (sumWrapped, string) Js_result.t

type tupleWrapped =
  { tw : ((int * string * float)) wrapper
  }

val encodeTupleWrapped : tupleWrapped -> Js_json.t

val decodeTupleWrapped : Js_json.t -> (tupleWrapped, string) Js_result.t

type 'a0 halfWrapped =
  { hw : ((int, 'a0) Aeson.Compatibility.Either.t) wrapper
  }

val encodeHalfWrapped : ('a0 -> Js_json.t) -> 'a0 halfWrapped -> Js_json.t

val decodeHalfWrapped : (Js_json.t -> ('a0, string) Js_result.t) -> Js_json.t -> ('a0 halfWrapped, string) Js_result.t

type ('a0, 'a1, 'a2) partiallyWrapped =
  { pw : ((int, (string * 'a1 * float * 'a2 * 'a0)) Aeson.Compatibility.Either.t) wrapper
  }

val encodePartiallyWrapped : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a0, 'a1, 'a2) partiallyWrapped -> Js_json.t

val decodePartiallyWrapped : (Js_json.t -> ('a0, string) Js_result.t) -> (Js_json.t -> ('a1, string) Js_result.t) -> (Js_json.t -> ('a2, string) Js_result.t) -> Js_json.t -> (('a0, 'a1, 'a2) partiallyWrapped, string) Js_result.t

type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs =
  { stprb : 'a1
  ; stprd : 'a3
  ; stpre : 'a4
  ; stpra : 'a0
  ; stprf : 'a5
  ; stprc : 'a2
  }

val encodeScrambledTypeParameterRefs : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a2 -> Js_json.t) -> ('a3 -> Js_json.t) -> ('a4 -> Js_json.t) -> ('a5 -> Js_json.t) -> ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs -> Js_json.t

val decodeScrambledTypeParameterRefs : (Js_json.t -> ('a0, string) Js_result.t) -> (Js_json.t -> ('a1, string) Js_result.t) -> (Js_json.t -> ('a2, string) Js_result.t) -> (Js_json.t -> ('a3, string) Js_result.t) -> (Js_json.t -> ('a4, string) Js_result.t) -> (Js_json.t -> ('a5, string) Js_result.t) -> Js_json.t -> (('a0, 'a1, 'a2, 'a3, 'a4, 'a5) scrambledTypeParameterRefs, string) Js_result.t

type wrappedWrapper =
  { ww : ((((int) option)) wrapper) option
  }

val encodeWrappedWrapper : wrappedWrapper -> Js_json.t

val decodeWrappedWrapper : Js_json.t -> (wrappedWrapper, string) Js_result.t
