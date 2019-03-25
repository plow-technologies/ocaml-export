type class =
  { subject : string
  ; students : (Person.person) list
  ; professor : Person.person
  }

val encodeClass : class -> Js_json.t

val decodeClass : Js_json.t -> (class, string) Belt.Result.t
