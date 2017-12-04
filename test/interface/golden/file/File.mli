type person =
  { id : int
  ; name : (string) option
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Js_result.t
