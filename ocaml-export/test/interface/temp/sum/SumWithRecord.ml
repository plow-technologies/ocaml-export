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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSumWithRecordA1: " ^ message)

let decodeSumWithRecordB2 json =
  match Aeson.Decode.
    { b2 = field "b2" string json
    ; b3 = field "b3" int json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSumWithRecordB2: " ^ message)

let decodeSumWithRecord json =
  match Aeson.Decode.(field "tag" string json) with
  | "A1" ->
     (match decodeSumWithRecordA1 json with
      | Belt.Result.Ok v -> Belt.Result.Ok (A1 v)
      | Belt.Result.Error message -> Belt.Result.Error ("decodeSumWithRecord: " ^ message)
     )
  | "B2" ->
     (match decodeSumWithRecordB2 json with
      | Belt.Result.Ok v -> Belt.Result.Ok (B2 v)
      | Belt.Result.Error message -> Belt.Result.Error ("decodeSumWithRecord: " ^ message)
     )
  | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message
