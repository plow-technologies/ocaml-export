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
