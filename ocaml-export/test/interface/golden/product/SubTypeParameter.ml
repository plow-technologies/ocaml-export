type ('a0, 'a1, 'a2) subTypeParameter =
  { listA : ('a0) list
  ; maybeB : ('a1) option
  ; tupleC : ('a2 * 'a1)
  }

let encodeSubTypeParameter encodeA0 encodeA1 encodeA2 x =
  Aeson.Encode.object_
    [ ( "listA", (Aeson.Encode.list encodeA0) x.listA )
    ; ( "maybeB", (Aeson.Encode.optional encodeA1) x.maybeB )
    ; ( "tupleC", (Aeson.Encode.pair encodeA2 encodeA1) x.tupleC )
    ]

let decodeSubTypeParameter decodeA0 decodeA1 decodeA2 json =
  match Aeson.Decode.
    { listA = field "listA" (list (fun a -> unwrapResult (decodeA0 a))) json
    ; maybeB = field "maybeB" (optional (fun a -> unwrapResult (decodeA1 a))) json
    ; tupleC = field "tupleC" (pair (fun a -> unwrapResult (decodeA2 a)) (fun a -> unwrapResult (decodeA1 a))) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSubTypeParameter: " ^ message)
