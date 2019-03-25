type a =
  | A of int

val encodeA : a -> Js_json.t

val decodeA : Js_json.t -> (a, string) Belt.Result.t

module One : sig


type b =
  | B of string * int

val encodeB : b -> Js_json.t

val decodeB : Js_json.t -> (b, string) Belt.Result.t

module Two : sig


type c =
  | C of (int * int)

val encodeC : c -> Js_json.t

val decodeC : Js_json.t -> (c, string) Belt.Result.t


end

type d =
  | D of string * Two.c

val encodeD : d -> Js_json.t

val decodeD : Js_json.t -> (d, string) Belt.Result.t


end

type e =
  | E of float

val encodeE : e -> Js_json.t

val decodeE : Js_json.t -> (e, string) Belt.Result.t
