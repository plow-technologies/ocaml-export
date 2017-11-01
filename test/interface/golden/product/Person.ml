type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

let encodePerson x =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "name", Json.Encode.optional Json.Encode.string x.name )
    ; ( "created", Json.Encode.date x.created )
    ]

let decodePerson json =
  match Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    ; created = field "created" date json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)
