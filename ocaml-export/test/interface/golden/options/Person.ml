type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

let encodePerson x =
  Aeson.Encode.object_
    [ ( "ID", Aeson.Encode.int x.id )
    ; ( "NAME", Aeson.Encode.optional Aeson.Encode.string x.name )
    ; ( "CREATED", Aeson.Encode.date x.created )
    ]

let decodePerson json =
  match Aeson.Decode.
    { id = field "ID" int json
    ; name = optional (field "NAME" string) json
    ; created = field "CREATED" date json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)
