type ('a0, 'a1, 'a2) subTypeParameter =
  { listA : ('a0) list
  ; maybeB : ('a1) option
  ; tupleC : ('a2 * 'a1)
  }

let encodeSubTypeParameter encodeA0 encodeA1 encodeA2 x =
  Json.Encode.object_
    [ ( "listA", (Json.Encode.list encodeA0) x.listA )
    ; ( "maybeB", Json.Encode.optional encodeA1 x.maybeB )
    ; ( "tupleC", Json.Encode.pair encodeA2 encodeA1 x.tupleC )
    ]

let decodeSubTypeParameter decodeA0 decodeA1 decodeA2 json =
  match Json.Decode.
    { listA = field "listA" (list (fun a -> unwrapResult (decodeA0 a))) json
    ; maybeB = optional (field "maybeB" (fun a -> unwrapResult (decodeA1 a))) json
    ; tupleC = field "tupleC" (pair (fun a -> unwrapResult (decodeA2 a)) (fun a -> unwrapResult (decodeA1 a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSubTypeParameter: " ^ message)
