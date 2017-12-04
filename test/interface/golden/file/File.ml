type person =
  { id : int
  ; name : (string) option
  }

let encodePerson x =
  Aeson.Encode.object_
    [ ( "id", Aeson.Encode.int x.id )
    ; ( "name", Aeson.Encode.optional Aeson.Encode.string x.name )
    ]

let decodePerson json =
  match Aeson.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

type automobile =
  { make : string
  ; model : string
  ; year : int
  }

let encodeAutomobile x =
  Aeson.Encode.object_
    [ ( "make", Aeson.Encode.string x.make )
    ; ( "model", Aeson.Encode.string x.model )
    ; ( "year", Aeson.Encode.int x.year )
    ]

let decodeAutomobile json =
  match Aeson.Decode.
    { make = field "make" string json
    ; model = field "model" string json
    ; year = field "year" int json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeAutomobile: " ^ message)

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
