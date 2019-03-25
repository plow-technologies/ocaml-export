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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSimple: " ^ message)
