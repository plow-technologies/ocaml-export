type complexProduct =
  { cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  }

let encodeComplexProduct x =
  Aeson.Encode.object_
    [ ( "cp1", (Aeson.Encode.list (Aeson.Encode.pair Aeson.Encode.int (Aeson.Encode.either Aeson.Encode.string Aeson.Encode.float))) x.cp1 )
    ]

let decodeComplexProduct json =
  match Aeson.Decode.
    { cp1 = field "cp1" (list (pair int (either string Aeson.Decode.float))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeComplexProduct: " ^ message)
