type school =
  { schoolName : string
  ; classes : (Class.class) list
  ; administrator : Person.person
  }

val encodeSchool : school -> Js_json.t

val decodeSchool : Js_json.t -> (school, string) Belt.Result.t
