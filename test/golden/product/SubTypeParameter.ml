type 'a0 subTypeParameter =
  { listA : ('a0) list
  }

let encodeSubTypeParameter (type a0) (encodeA0 : a0 -> Js_json.t) (x : a0 subTypeParameter) :Js_json.t =
  Json.Encode.object_
    [ ( "listA", (Json.Encode.list encodeA0) x.listA )
    ]

let decodeSubTypeParameter (type a0) (decodeA0 : Js_json.t -> (a0, string) Js_result.t) (json : Js_json.t) :(a0 subTypeParameter, string) Js_result.t =
  match Json.Decode.
    { listA = field "listA" (list (fun a -> unwrapResult (decodeA0 a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSubTypeParameter: " ^ message)
