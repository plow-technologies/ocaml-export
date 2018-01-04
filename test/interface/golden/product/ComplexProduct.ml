type complexProduct =
  { cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  ; cp2 : ((int) list) list
  ; cp3 : ((int) list) option
  }

let encodeComplexProduct x =
  Aeson.Encode.object_
    [ ( "cp1", (Aeson.Encode.list (Aeson.Encode.pair Aeson.Encode.int (Aeson.Encode.either Aeson.Encode.string Aeson.Encode.float))) x.cp1 )
    ; ( "cp2", (Aeson.Encode.list (Aeson.Encode.list Aeson.Encode.int)) x.cp2 )
    ; ( "cp3", (Aeson.Encode.optional (Aeson.Encode.list Aeson.Encode.int)) x.cp3 )
    ]

let decodeComplexProduct json =
  match Aeson.Decode.
    { cp1 = field "cp1" (list (pair int (either string Aeson.Decode.float))) json
    ; cp2 = field "cp2" (list (list int)) json
    ; cp3 = optional (field "cp3" (list int)) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeComplexProduct: " ^ message)
