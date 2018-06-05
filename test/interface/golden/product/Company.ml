type company =
  { address : string
  ; employees : (Person.person) list
  }

let encodeCompany x =
  Aeson.Encode.object_
    [ ( "address", Aeson.Encode.string x.address )
    ; ( "employees", (Aeson.Encode.list Person.encodePerson) x.employees )
    ]

let decodeCompany json =
  match Aeson.Decode.
    { address = field "address" string json
    ; employees = field "employees" (list (fun a -> unwrapResult (Person.decodePerson a))) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeCompany: " ^ message)
