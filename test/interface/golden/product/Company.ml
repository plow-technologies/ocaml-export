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
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeCompany: " ^ message)
