type simple =
  { sa : int
  ; sb : string
  }

let encodeSimple x =
  Aeson.Encode.object_
    [ ( "sa", Aeson.Encode.int x.sa )
    ; ( "sb", Aeson.Encode.string x.sb )
    ]

let decodeSimple json =
  match Aeson.Decode.
    { sa = field "sa" int json
    ; sb = field "sb" string json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeSimple: " ^ message)


type complexProduct =
  { cp0 : (Person.person, (int) list) Aeson.Compatibility.Either.t
  ; cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  ; cp2 : ((int) list) list
  ; cp3 : ((int) list) option
  ; cp4 : (simple, int) Aeson.Compatibility.Either.t
  }

let encodeComplexProduct x =
  Aeson.Encode.object_
    [ ( "cp0", (Aeson.Encode.either Person.encodePerson (Aeson.Encode.list Aeson.Encode.int)) x.cp0 )
    ; ( "cp1", (Aeson.Encode.list (Aeson.Encode.tuple2 Aeson.Encode.int (Aeson.Encode.either Aeson.Encode.string Aeson.Encode.float))) x.cp1 )
    ; ( "cp2", (Aeson.Encode.list (Aeson.Encode.list Aeson.Encode.int)) x.cp2 )
    ; ( "cp3", (Aeson.Encode.optional (Aeson.Encode.list Aeson.Encode.int)) x.cp3 )
    ; ( "cp4", (Aeson.Encode.either encodeSimple Aeson.Encode.int) x.cp4 )
    ]

let decodeComplexProduct json =
  match Aeson.Decode.
    { cp0 = field "cp0" (either (fun a -> unwrapResult (Person.decodePerson a)) (list int)) json
    ; cp1 = field "cp1" (list (tuple2 int (either string Aeson.Decode.float))) json
    ; cp2 = field "cp2" (list (list int)) json
    ; cp3 = optional (field "cp3" (list int)) json
    ; cp4 = field "cp4" (either (fun a -> unwrapResult (decodeSimple a)) int) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeComplexProduct: " ^ message)
