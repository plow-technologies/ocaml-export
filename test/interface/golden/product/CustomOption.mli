type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Js_result.t

type company2 =
  { address2 : string
  ; boss : (person) option
  }

val encodeCompany2 : company2 -> Js_json.t

val decodeCompany2 : Js_json.t -> (company2, string) Js_result.t
