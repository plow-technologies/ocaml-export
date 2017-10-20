type person =
  { id : int
  ; name : (string) option
  }

let encodePerson (x : person) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "name", (fun a -> Option.default Json.Encode.null (Option.map Json.Encode.string a)) x.name )
    ]

let decodePerson (json : Js_json.t) :(person, string) Js_result.t =
  match Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

type company =
  { address : string
  ; employees : (person) list
  }

let encodeCompany (x : company) :Js_json.t =
  Json.Encode.object_
    [ ( "address", Json.Encode.string x.address )
    ; ( "employees", (Json.Encode.list encodePerson) x.employees )
    ]

let decodeCompany (json : Js_json.t) :(company, string) Js_result.t =
  match Json.Decode.
    { address = field "address" string json
    ; employees = field "employees" (list (fun a -> unwrapResult (decodePerson a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeCompany: " ^ message)
