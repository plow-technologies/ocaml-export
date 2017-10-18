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

let encodeA1 (x : sumWithRecordA1) :Js_json.t =
  Json.Encode.object_
    [ ( "a1" ; Json.Encode.int x.a1 )
    ]

let encodeB2 (x : sumWithRecordB2) :Js_json.t =
  Json.Encode.object_
    [ ( "b2" ; Json.Encode.string x.b2 )
    ; ( "b3" ; Json.Encode.int x.b3 )
    ]

let encodeSumWithRecord (x : sumWithRecord) :Js_json.t =
  match x with
  | A1 y0 ->
     (match (Js.Json.decodeObject (encodeA1 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "A1");
         Js.Json.object_ dict;
      | None ->
         Json.Encode.object_ []
     )
  | B2 y0 ->
     (match (Js.Json.decodeObject (encodeB2 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "B2");
         Js.Json.object_ dict;
      | None ->
         Json.Encode.object_ []
     )
