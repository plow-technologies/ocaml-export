type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Js_result.t

type company =
  { address : string
  ; employees : (person) list
  }

val encodeCompany : company -> Js_json.t

val decodeCompany : Js_json.t -> (company, string) Js_result.t
