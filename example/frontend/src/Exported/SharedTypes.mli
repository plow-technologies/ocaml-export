type ('key, 'value) entity =
  { entityKey   : 'key
  ; entityValue : 'value
  }

val encodeEntity : ('key -> Js_json.t) -> ('value -> Js_json.t) -> ('key, 'value) entity -> Js_json.t

val decodeEntity : (Js_json.t -> ('key, string) Belt.Result.t) -> (Js_json.t -> ('value, string) Belt.Result.t) -> Js_json.t -> (('key, 'value) entity, string) Belt.Result.t


type key =
  | Key of int

val encodeKey : key -> Js_json.t

val decodeKey : Js_json.t -> (key, string) Belt.Result.t

type todoId =
  | TodoId of key

val encodeTodoId : todoId -> Js_json.t

val decodeTodoId : Js_json.t -> (todoId, string) Belt.Result.t

type userId =
  | UserId of key

val encodeUserId : userId -> Js_json.t

val decodeUserId : Js_json.t -> (userId, string) Belt.Result.t

type username =
  | Username of string

val encodeUsername : username -> Js_json.t

val decodeUsername : Js_json.t -> (username, string) Belt.Result.t

type todo =
  { description : string
  ; completed : bool
  ; created : Js_date.t
  ; madeBy : userId
  }

val encodeTodo : todo -> Js_json.t

val decodeTodo : Js_json.t -> (todo, string) Belt.Result.t

type user =
  { username : username
  ; password : string
  }

val encodeUser : user -> Js_json.t

val decodeUser : Js_json.t -> (user, string) Belt.Result.t
