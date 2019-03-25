type person =
  { id : int
  ; name : (string) option
  }

let encodePerson x =
  Aeson.Encode.object_
    [ ( "id", Aeson.Encode.int x.id )
    ; ( "name", Aeson.Encode.optional Aeson.Encode.string x.name )
    ]

let decodePerson json =
  match Aeson.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePerson: " ^ message)
