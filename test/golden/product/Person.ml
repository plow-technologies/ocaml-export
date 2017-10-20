type person =
  { id : int
  ; name : (string) option
  }

let encodePerson (x : person) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "name", (fun a -> Option.default Json.Encode.null (Option.map Json.Encode.string a)) x.name )
    ]

let decodePerson (json : Js_json.t) :person option =
  match Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Some v
  | exception Json.Decode.DecodeError _ -> None
