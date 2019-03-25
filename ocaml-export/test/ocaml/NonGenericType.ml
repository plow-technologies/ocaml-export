type nonGenericType =
  { ngA : string
  ; ngB : int
  }

let encodeNonGenericType x =
  Aeson.Encode.object_
    [ ( "ngA", Aeson.Encode.string x.ngA)
    ; ( "ngB", Aeson.Encode.int x.ngB)
    ]

let decodeNonGenericType json =
  match Aeson.Decode.
        { ngA = field "ngA" string json
        ; ngB = field "ngB" int json
        }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeNonGenericType: " ^ message)
