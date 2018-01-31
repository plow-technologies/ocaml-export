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

type sumWrapped =
  | SW1
  | SW2 of (int) wrapper
  | SW3 of ((string) option) wrapper
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
       ; ( "contents", encodeWrapper Aeson.Encode.int y0 )
       ]
  | SW3 y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW3" )
       ; ( "contents", encodeWrapper (Aeson.Encode.optional (Aeson.Encode.string)) y0 )
       ]
  | SW4 y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "SW4" )
       ; ( "contents", encodeWrapper (Aeson.Encode.either Aeson.Encode.int (Aeson.Encode.string)) y0 )
       ]

let decodeSumWrapped json =
  match Aeson.Decode.(field "tag" string json) with
  | "SW1" ->
     Js_result.Ok SW1

  | "SW2" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult int) a)) json) with
      | v -> Js_result.Ok (SW2 v)
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("SW2: " ^ message)
     )
  | "SW3" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult (optional (string))) a)) json) with
      | v -> Js_result.Ok (SW3 v)
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("SW3: " ^ message)
     )

  | "SW4" ->
     (match Aeson.Decode.(field "contents" (fun a -> unwrapResult (decodeWrapper (wrapResult (either int (string))) a)) json) with
      | v -> Js_result.Ok (SW4 v)
      | exception Aeson.Decode.DecodeError message -> Js_result.Error ("SW4: " ^ message)
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Js_result.Error message