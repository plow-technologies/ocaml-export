type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

let encodePerson x =
  Aeson.Encode.object_
    [ ( "id", Aeson.Encode.int x.id )
    ; ( "name", Aeson.Encode.optional Aeson.Encode.string x.name )
    ; ( "created", Aeson.Encode.date x.created )
    ]

let decodePerson json =
  match Aeson.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    ; created = field "created" date json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

type company =
  { address : string
  ; employees : (person) list
  }

let encodeCompany x =
  Aeson.Encode.object_
    [ ( "address", Aeson.Encode.string x.address )
    ; ( "employees", (Aeson.Encode.list encodePerson) x.employees )
    ]

let decodeCompany json =
  match Aeson.Decode.
    { address = field "address" string json
    ; employees = field "employees" (list (fun a -> unwrapResult (decodePerson a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeCompany: " ^ message)
