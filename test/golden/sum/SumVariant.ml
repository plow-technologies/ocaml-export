type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)
  | HasMixed of int * string * float

let encodeSumVariant (x : sumVariant) :Js_json.t =
  match x with
  | HasNothing ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasNothing" )
       ]
  | HasSingleInt y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasSingleInt" )
       ; ( "contents", Json.Encode.int y0 )
       ]
  | HasSingleTuple y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasSingleTuple" )
       ; ( "contents", (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0 )
       ]
  | HasMultipleInts (y0,y1) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasMultipleInts" )
       ; ( "contents", Json.Encode.array [| Json.Encode.int y0 ; Json.Encode.int y1 |] )
       ]
  | HasMultipleTuples (y0,y1) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasMultipleTuples" )
       ; ( "contents", Json.Encode.array [| (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0 ; (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y1 |] )
       ]
  | HasMixed (y0,y1,y2) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasMixed" )
       ; ( "contents", Json.Encode.array [| Json.Encode.int y0 ; Json.Encode.string y1 ; Json.Encode.float y2 |] )
       ]

let decodeSumVariant (json : Js_json.t) :(sumVariant, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "HasNothing" ->
     Js_result.Ok HasNothing

  | "HasSingleInt" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (HasSingleInt v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("HasSingleInt: " ^ message)
     )

  | "HasSingleTuple" ->
     (match Json.Decode.(field "contents" (pair int int) json) with
      | v -> Js_result.Ok (HasSingleTuple v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("HasSingleTuple: " ^ message)
     )
  | "HasMultipleInts" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.int v.(0) with
          | v0 ->
             (match Json.Decode.int v.(1) with
              | v1 ->
                 Js_result.Ok (HasMultipleInts (v0, v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMultipleInts: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMultipleInts: " ^ message)
         )
      | None -> Js_result.Error ("HasMultipleInts expected an array.")
     )

  | "HasMultipleTuples" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.(pair int int) v.(0) with
          | v0 ->
             (match Json.Decode.(pair int int) v.(1) with
              | v1 ->
                 Js_result.Ok (HasMultipleTuples (v0, v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMultipleTuples: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMultipleTuples: " ^ message)
         )
      | None -> Js_result.Error ("HasMultipleTuples expected an array.")
     )

  | "HasMixed" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.int v.(0) with
          | v0 ->
             (match Json.Decode.string v.(1) with
              | v1 ->
                 (match Json.Decode.float v.(2) with
                  | v2 ->
                     Js_result.Ok (HasMixed (v0, v1, v2))
                  | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMixed: " ^ message)
                 )
              | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMixed: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("HasMixed: " ^ message)
         )
      | None -> Js_result.Error ("HasMixed expected an array.")
     )
  | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error message
