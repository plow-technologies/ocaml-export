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
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

