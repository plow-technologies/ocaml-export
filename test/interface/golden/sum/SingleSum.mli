type singleSum =
  | SingleSum

val encodeSingleSum : singleSum -> Js_json.t

val decodeSingleSum : Js_json.t -> (singleSum, string) Belt.Result.t
