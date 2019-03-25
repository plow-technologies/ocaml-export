type sqlKey =
  | SqlKey of int

val encodeSqlKey : sqlKey -> Js_json.t

val decodeSqlKey : Js_json.t -> (sqlKey, string) Belt.Result.t

type userId =
  | UserId of sqlKey

val encodeUserId : userId -> Js_json.t

val decodeUserId : Js_json.t -> (userId, string) Belt.Result.t
