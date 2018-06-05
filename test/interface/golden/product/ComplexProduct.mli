type simple =
  { sa : int
  ; sb : string
  }

val encodeSimple : simple -> Js_json.t

val decodeSimple : Js_json.t -> (simple, string) Belt.Result.t


type complexProduct =
  { cp0 : (Person.person, (int) list) Aeson.Compatibility.Either.t
  ; cp1 : ((int * (string, float) Aeson.Compatibility.Either.t)) list
  ; cp2 : ((int) list) list
  ; cp3 : ((int) list) option
  ; cp4 : (simple, int) Aeson.Compatibility.Either.t
  }

val encodeComplexProduct : complexProduct -> Js_json.t

val decodeComplexProduct : Js_json.t -> (complexProduct, string) Belt.Result.t
