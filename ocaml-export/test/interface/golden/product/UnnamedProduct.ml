type unnamedProduct =
  | UnnamedProduct of string * int

let encodeUnnamedProduct x =
  match x with
  | UnnamedProduct (y0,y1) ->
     Aeson.Encode.array [| Aeson.Encode.string y0 ; Aeson.Encode.int y1 |]

let decodeUnnamedProduct json =
  match Js.Json.decodeArray json with
  | Some v ->
     (match Aeson.Decode.(string) v.(0) with
      | v0 ->
         (match Aeson.Decode.(int) v.(1) with
          | v1 ->
             Belt.Result.Ok (UnnamedProduct (v0, v1))
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("UnnamedProduct: " ^ message)
         )
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("UnnamedProduct: " ^ message)
     )
  | None -> Belt.Result.Error ("UnnamedProduct expected an array.")
