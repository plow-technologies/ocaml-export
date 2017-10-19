type person =
  { id : int
  ; name : (string) option
  }

let encodePerson (x : person) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "name", (fun a -> Option.default Json.Encode.null (Option.map Json.Encode.string a)) x.name )
    ]
