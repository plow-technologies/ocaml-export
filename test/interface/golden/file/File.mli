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
