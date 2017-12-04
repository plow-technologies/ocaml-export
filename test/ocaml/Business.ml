type business =
  { taxId : string
  ; owner : person
  ; employees : (person) list
  ; companyVehicle : (automobile) option
  }

let encodeBusiness x =
  Aeson.Encode.object_
    [ ( "taxId", Aeson.Encode.string x.taxId )
    ; ( "owner", encodePerson x.owner )
    ; ( "employees", (Aeson.Encode.list encodePerson) x.employees )
    ; ( "companyVehicle", Aeson.Encode.optional encodeAutomobile x.companyVehicle )
    ]

let decodeBusiness json =
  match Aeson.Decode.
    { taxId = field "taxId" string json
    ; owner = field "owner" (fun a -> unwrapResult (decodePerson a)) json
    ; employees = field "employees" (list (fun a -> unwrapResult (decodePerson a))) json
    ; companyVehicle = optional (field "companyVehicle" (fun a -> unwrapResult (decodeAutomobile a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeBusiness: " ^ message)
