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
  { mw : ((int) option) wrapper
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
