type sqlKey =
  | SqlKey of int

let encodeSqlKey x =
  match x with
  | SqlKey y0 ->
     Aeson.Encode.int y0

let decodeSqlKey json =
  match Aeson.Decode.int json with
  | v -> Belt.Result.Ok (SqlKey v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeSqlKey: " ^ msg)

type userId =
  | UserId of sqlKey

let encodeUserId x =
  match x with
  | UserId y0 ->
     encodeSqlKey y0

let decodeUserId json =
  match (Aeson.Decode.unwrapResult (decodeSqlKey json)) with
  | v -> Belt.Result.Ok (UserId v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeUserId: " ^ msg)
