type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

let encodePerson x =
  Aeson.Encode.object_
    [ ( "id", Aeson.Encode.int x.id )
    ; ( "name", (Aeson.Encode.optional Aeson.Encode.string) x.name )
    ; ( "created", Aeson.Encode.date x.created )
    ]

let decodePerson json =
  match Aeson.Decode.
    { id = field "id" int json
    ; name = field "name" (optional string) json
    ; created = field "created" date json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePerson: " ^ message)
