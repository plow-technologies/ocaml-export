type ('a, 'b) wrapper =
  { wrapperA : 'a
  ; wrapperB : 'b
  ; wrapperC : string
  }

let encodeWrapper a b x =
  Aeson.Encode.object_
    [ ( "wrapperA", a x.wrapperA )
    ; ( "wrapperB", b x.wrapperB )
    ; ( "wrapperC", Aeson.Encode.string x.wrapperC )
    ]

let decodeWrapper a b json =
  match Aeson.Decode.
        { wrapperA = field "wrapperA" (fun x -> unwrapResult (a x)) json
        ; wrapperB = field "wrapperB" (fun x -> unwrapResult (b x)) json
        ; wrapperC = field "wrapperC" string json
        }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeWrapper: " ^ message)
