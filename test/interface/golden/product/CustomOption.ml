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

type company2 =
  { address2 : string
  ; boss : (person) option
  }

let encodeCompany2 x =
  Aeson.Encode.object_
    [ ( "address2", Aeson.Encode.string x.address2 )
    ; ( "boss", Aeson.Encode.optional encodePerson x.boss )
    ]

let decodeCompany2 json =
  match Aeson.Decode.
    { address2 = field "address2" string json
    ; boss = optional (field "boss" (fun a -> unwrapResult (decodePerson a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeCompany2: " ^ message)
