type person =
  { id : int
  ; name : (string) option
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Belt.Result.t


type automobile =
  { make : string
  ; model : string
  ; year : int
  }

val encodeAutomobile : automobile -> Js_json.t

val decodeAutomobile : Js_json.t -> (automobile, string) Belt.Result.t

type business =
  { taxId : string
  ; owner : person
  ; employees : (person) list
  ; companyVehicle : (automobile) option
  }

val encodeBusiness : business -> Js_json.t

val decodeBusiness : Js_json.t -> (business, string) Belt.Result.t


type ('a, 'b) wrapper =
  { wrapperA : 'a
  ; wrapperB : 'b
  ; wrapperC : string
  }

val encodeWrapper : ('a -> Js_json.t) -> ('b -> Js_json.t) -> ('a, 'b) wrapper -> Js_json.t

val decodeWrapper : (Js_json.t -> ('a, string) Belt.Result.t) -> (Js_json.t -> ('b, string) Belt.Result.t) -> Js_json.t -> (('a, 'b) wrapper, string) Belt.Result.t


type autoDependingOnManual =
  { abc : string
  ; bbBusiness : business
  }

val encodeAutoDependingOnManual : autoDependingOnManual -> Js_json.t

val decodeAutoDependingOnManual : Js_json.t -> (autoDependingOnManual, string) Belt.Result.t

type nonGenericType =
  { ngA : string
  ; ngB : int
  }

val encodeNonGenericType : nonGenericType -> Js_json.t

val decodeNonGenericType : Js_json.t -> (nonGenericType, string) Belt.Result.t

