type person =
  { id : int
  ; name : (string) option
  ; created : Js_date.t
  }

val encodePerson : person -> Js_json.t

val decodePerson : Js_json.t -> (person, string) Belt.Result.t
