type simpleChoice =
  { choice : (string, int) Aeson.Compatibility.Either.t
  }

let encodeSimpleChoice x =
  Aeson.Encode.object_
    [ ( "choice", (Aeson.Encode.either Aeson.Encode.string Aeson.Encode.int) x.choice )
    ]

let decodeSimpleChoice json =
  match Aeson.Decode.
    { choice = field "choice" (either string int) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSimpleChoice: " ^ message)
