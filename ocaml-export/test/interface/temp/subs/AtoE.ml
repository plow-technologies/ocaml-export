type a =
  | A of int

let encodeA x =
  match x with
  | A y0 ->
     Aeson.Encode.int y0

let decodeA json =
  match Aeson.Decode.int json with
  | v -> Belt.Result.Ok (A v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeA: " ^ msg)

module One = struct


type b =
  | B of string * int

let encodeB x =
  match x with
  | B (y0,y1) ->
     Aeson.Encode.array [| Aeson.Encode.string y0 ; Aeson.Encode.int y1 |]

let decodeB json =
  match Js.Json.decodeArray json with
  | Some v ->
     (match Aeson.Decode.(string) v.(0) with
      | v0 ->
         (match Aeson.Decode.(int) v.(1) with
          | v1 ->
             Belt.Result.Ok (B (v0, v1))
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("B: " ^ message)
         )
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("B: " ^ message)
     )
  | None -> Belt.Result.Error ("B expected an array.")

module Two = struct


type c =
  | C of (int * int)

let encodeC x =
  match x with
  | C y0 ->
     (Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int) y0

let decodeC json =
  match Aeson.Decode.(pair int int) json with
  | v -> Belt.Result.Ok (C v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeC: " ^ msg)


end

type d =
  | D of string * Two.c

let encodeD x =
  match x with
  | D (y0,y1) ->
     Aeson.Encode.array [| Aeson.Encode.string y0 ; Two.encodeC y1 |]

let decodeD json =
  match Js.Json.decodeArray json with
  | Some v ->
     (match Aeson.Decode.(string) v.(0) with
      | v0 ->
         (match Aeson.Decode.((fun a -> unwrapResult (Two.decodeC a))) v.(1) with
          | v1 ->
             Belt.Result.Ok (D (v0, v1))
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("D: " ^ message)
         )
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("D: " ^ message)
     )
  | None -> Belt.Result.Error ("D expected an array.")


end

type e =
  | E of float

let encodeE x =
  match x with
  | E y0 ->
     Aeson.Encode.float y0

let decodeE json =
  match Aeson.Decode.Aeson.Decode.float json with
  | v -> Belt.Result.Ok (E v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeE: " ^ msg)
