type business =
  { taxId : string
  ; owner : person
  ; employees : (person) list
  ; companyVehicle : (automobile) option
  }

val encodeBusiness : business -> Js_json.t

val decodeBusiness : Js_json.t -> (business, string) Js_result.t
