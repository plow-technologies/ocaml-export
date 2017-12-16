type a =
  | A of int

let encodeA x =
  match x with
  | A y0 ->
     Aeson.Encode.int y0

let decodeA json =
  match Aeson.Decode.int json with
  | v -> Js_result.Ok (A v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeA: " ^ msg)

module One = struct


type b =
  | B of string * int

let encodeB x =
  match x with
  | B (y0,y1) ->
     Aeson.Encode.array [| Aeson.Encode.string y0 ; Aeson.Encode.int y1 |]

let decodeB json =
  match Aeson.Decode.string
  ; int json with
  | v -> Js_result.Ok (B v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeB: " ^ msg)

module Two = struct


type c =
  | C of (int * int)

let encodeC x =
  match x with
  | C y0 ->
     Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int y0

let decodeC json =
  match Aeson.Decode.(pair int int) json with
  | v -> Js_result.Ok (C v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeC: " ^ msg)


end

type d =
  | D of string * Two.c

let encodeD x =
  match x with
  | D (y0,y1) ->
     Aeson.Encode.array [| Aeson.Encode.string y0 ; Two.encodeC y1 |]

let decodeD json =
  match Aeson.Decode.string
  ; (fun a -> unwrapResult (Two.decodeC a)) json with
  | v -> Js_result.Ok (D v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeD: " ^ msg)


end

type e =
  | E of float

let encodeE x =
  match x with
  | E y0 ->
     Aeson.Encode.float y0

let decodeE json =
  match Aeson.Decode.float json with
  | v -> Js_result.Ok (E v)
  | exception Aeson.Decode.DecodeError msg -> Js_result.Error ("decodeE: " ^ msg)
