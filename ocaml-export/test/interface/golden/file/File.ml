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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePerson: " ^ message)


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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeAutomobile: " ^ message)

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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeBusiness: " ^ message)


type ('a, 'b) wrapper =
  { wrapperA : 'a
  ; wrapperB : 'b
  ; wrapperC : string
  }

let encodeWrapper a b x =
  Aeson.Encode.object_
    [ ( "wrapperA", a x.wrapperA )
    ; ( "wrapperB", b x.wrapperB )
    ; ( "wrapperC", Aeson.Encode.string x.wrapperC )
    ]

let decodeWrapper a b json =
  match Aeson.Decode.
        { wrapperA = field "wrapperA" (fun x -> unwrapResult (a x)) json
        ; wrapperB = field "wrapperB" (fun x -> unwrapResult (b x)) json
        ; wrapperC = field "wrapperC" string json
        }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeWrapper: " ^ message)


type autoDependingOnManual =
  { abc : string
  ; bbBusiness : business
  }

let encodeAutoDependingOnManual x =
  Aeson.Encode.object_
    [ ( "abc", Aeson.Encode.string x.abc )
    ; ( "bbBusiness", encodeBusiness x.bbBusiness )
    ]

let decodeAutoDependingOnManual json =
  match Aeson.Decode.
    { abc = field "abc" string json
    ; bbBusiness = field "bbBusiness" (fun a -> unwrapResult (decodeBusiness a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeAutoDependingOnManual: " ^ message)

type nonGenericType =
  { ngA : string
  ; ngB : int
  }

let encodeNonGenericType x =
  Aeson.Encode.object_
    [ ( "ngA", Aeson.Encode.string x.ngA)
    ; ( "ngB", Aeson.Encode.int x.ngB)
    ]

let decodeNonGenericType json =
  match Aeson.Decode.
        { ngA = field "ngA" string json
        ; ngB = field "ngB" int json
        }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeNonGenericType: " ^ message)

