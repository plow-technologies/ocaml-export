type simple =
  { sa : int
  ; sb : string
  }

val encodeSimple : simple -> Js_json.t

val decodeSimple : Js_json.t -> (simple, string) Belt.Result.t


type complexProduct =
  { cp0 : ((int) list, Person.person) Belt.Result.t
  ; cp1 : ((int * (float, string) Belt.Result.t)) list
  ; cp2 : ((int) list) list
  ; cp3 : ((int) list) option
  ; cp4 : (int, simple) Belt.Result.t
  }

val encodeComplexProduct : complexProduct -> Js_json.t

val decodeComplexProduct : Js_json.t -> (complexProduct, string) Belt.Result.t
