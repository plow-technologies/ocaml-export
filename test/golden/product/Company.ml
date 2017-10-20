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

type company =
  { address : string
  ; employees : (person) list
  }

let encodeCompany (x : company) :Js_json.t =
  Json.Encode.object_
    [ ( "address", Json.Encode.string x.address )
    ; ( "employees", (Json.Encode.list encodePerson) x.employees )
    ]

let decodeCompany (json : Js_json.t) :person option =
  match Json.Decode.
    { address = field "address" string json
    ; name = list (field "employees" person) json
    }
  with
  | v -> Some v
  | exception Json.Decode.DecodeError _ -> None
