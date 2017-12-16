type company =
  { address : string
  ; employees : (Person.person) list
  }

val encodeCompany : company -> Js_json.t

val decodeCompany : Js_json.t -> (company, string) Js_result.t
