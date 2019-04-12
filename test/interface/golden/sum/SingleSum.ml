type singleSum =
  | SingleSum

let encodeSingleSum x =
  match x with
  | SingleSum ->
     Aeson.Encode.list Aeson.Encode.int []

let decodeSingleSum json =
  match (Aeson.Decode.(list int json)) with
  | _ -> Belt.Result.Ok SingleSum
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSingleSum: expected a top-level empty JSON array. Got: " ^ message)
