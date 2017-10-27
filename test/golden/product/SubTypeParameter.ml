type ('a0, 'a1, 'a2) subTypeParameter =
  { listA : ('a0) list
  ; maybeB : ('a1) option
  ; tupleC : ('a2 * int)
  }

let encodeSubTypeParameter (type a0) (type a1) (type a2) (encodeA0 : a0 -> Js_json.t) (encodeA1 : a1 -> Js_json.t) (encodeA2 : a2 -> Js_json.t) (x : (a0, a1, a2) subTypeParameter) :Js_json.t =
  Json.Encode.object_
    [ ( "listA", (Json.Encode.list encodeA0) x.listA )
    ; ( "maybeB", (fun a -> Option.default Json.Encode.null (Option.map encodeA1 a)) x.maybeB )
    ; ( "tupleC", (fun (a,b) -> Json.Encode.array [| encodeA2 a ; Json.Encode.int b  |]) x.tupleC )
    ]

let decodeSubTypeParameter (type a0) (type a1) (type a2) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (decodeA1 : Js_json.t -> (a1, string) Js_result.t) (decodeA2 : Js_json.t -> (a2, string) Js_result.t) (json : Js_json.t) :((a0, a1, a2) subTypeParameter, string) Js_result.t =
  match Json.Decode.
    { listA = field "listA" (list (fun a -> unwrapResult (decodeA0 a))) json
    ; maybeB = optional (field "maybeB" (fun a -> unwrapResult (decodeA1 a))) json
    ; tupleC = field "tupleC" (pair (fun a -> unwrapResult (decodeA2 a)) int) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSubTypeParameter: " ^ message)
