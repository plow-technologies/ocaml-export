type company2 =
  { address2 : string
  ; boss : (Person.person) option
  }

val encodeCompany2 : company2 -> Js_json.t

val decodeCompany2 : Js_json.t -> (company2, string) Js_result.t
