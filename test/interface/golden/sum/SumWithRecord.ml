type sumWithRecordA1 =
  { a1 : int
  }

type sumWithRecordB2 =
  { b2 : string
  ; b3 : int
  }

type sumWithRecord =
  | A1 of sumWithRecordA1
  | B2 of sumWithRecordB2

let encodeSumWithRecordA1 x =
  Aeson.Encode.object_
    [ ( "a1", Aeson.Encode.int x.a1 )
    ]

let encodeSumWithRecordB2 x =
  Aeson.Encode.object_
    [ ( "b2", Aeson.Encode.string x.b2 )
    ; ( "b3", Aeson.Encode.int x.b3 )
    ]

let encodeSumWithRecord x =
  match x with
  | A1 y0 ->
     (match (Js.Json.decodeObject (encodeSumWithRecordA1 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "A1");
         Js.Json.object_ dict
      | None ->
         Aeson.Encode.object_ []
     )
  | B2 y0 ->
     (match (Js.Json.decodeObject (encodeSumWithRecordB2 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "B2");
         Js.Json.object_ dict
      | None ->
         Aeson.Encode.object_ []
     )

let decodeSumWithRecordA1 json =
  match Aeson.Decode.
    { a1 = field "a1" int json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeSumWithRecordA1: " ^ message)

let decodeSumWithRecordB2 json =
  match Aeson.Decode.
    { b2 = field "b2" string json
    ; b3 = field "b3" int json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeSumWithRecordB2: " ^ message)

let decodeSumWithRecord json =
  match Aeson.Decode.(field "tag" string json) with
  | "A1" ->
     (match decodeSumWithRecordA1 json with
      | Js_result.Ok v -> Js_result.Ok (A1 v)
      | Js_result.Error message -> Js_result.Error ("decodeSumWithRecord: " ^ message)
     )
  | "B2" ->
     (match decodeSumWithRecordB2 json with
      | Js_result.Ok v -> Js_result.Ok (B2 v)
      | Js_result.Error message -> Js_result.Error ("decodeSumWithRecord: " ^ message)
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Js_result.Error message
