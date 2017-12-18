type person =
  { id : int
  ; name : (string) option
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Js_result.t


type automobile =
  { make : string
  ; model : string
  ; year : int
  }

val encodeAutomobile : automobile -> Js_json.t

val decodeAutomobile : Js_json.t -> (automobile, string) Js_result.t

type business =
  { taxId : string
  ; owner : person
  ; employees : (person) list
  ; companyVehicle : (automobile) option
  }

val encodeBusiness : business -> Js_json.t

val decodeBusiness : Js_json.t -> (business, string) Js_result.t


type ('a, 'b) wrapper =
  { wrapperA : 'a
  ; wrapperB : 'b
  ; wrapperC : string
  }

val encodeWrapper : ('a -> Js_json.t) -> ('b -> Js_json.t) -> ('a, 'b) wrapper -> Js_json.t

val decodeWrapper : (Js_json.t -> ('a, string) Js_result.t) -> (Js_json.t -> ('b, string) Js_result.t) -> Js_json.t -> (('a, 'b) wrapper, string) Js_result.t

