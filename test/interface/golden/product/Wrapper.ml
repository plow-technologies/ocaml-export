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
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeWrapper: " ^ message)

type intWrapped =
  { iw : (int) wrapper
  }

let encodeIntWrapped x =
  Aeson.Encode.object_
    [ ( "iw", encodeWrapper Aeson.Encode.int x.iw )
    ]

let decodeIntWrapped json =
  match Aeson.Decode.
    { iw = field "iw" (fun a -> unwrapResult (decodeWrapper (wrapResult int) a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeIntWrapped: " ^ message)

type maybeWrapped =
  { mw : ((int) option) wrapper
  }

let encodeMaybeWrapped x =
  Aeson.Encode.object_
    [ ( "mw", encodeWrapper (Aeson.Encode.optional Aeson.Encode.int) x.mw )
    ]

let decodeMaybeWrapped json =
  match Aeson.Decode.
    { mw = field "mw" (fun a -> unwrapResult (decodeWrapper (wrapResult (optional int)) a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeMaybeWrapped: " ^ message)

type eitherWrapped =
  { ew : ((int, float) Aeson.Compatibility.Either.t) wrapper
  }

let encodeEitherWrapped x =
  Aeson.Encode.object_
    [ ( "ew", encodeWrapper (Aeson.Encode.either Aeson.Encode.int Aeson.Encode.float) x.ew )
    ]

let decodeEitherWrapped json =
  match Aeson.Decode.
    { ew = field "ew" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int Aeson.Decode.float)) a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeEitherWrapped: " ^ message)

type complexWrapped =
  { cw : (((string) option, float) Aeson.Compatibility.Either.t) wrapper
  }

let encodeComplexWrapped x =
  Aeson.Encode.object_
    [ ( "cw", encodeWrapper (Aeson.Encode.either (Aeson.Encode.optional Aeson.Encode.string) Aeson.Encode.float) x.cw )
    ]

let decodeComplexWrapped json =
  match Aeson.Decode.
    { cw = field "cw" (fun a -> unwrapResult (decodeWrapper (wrapResult (either (optional string) Aeson.Decode.float)) a)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeComplexWrapped: " ^ message)
