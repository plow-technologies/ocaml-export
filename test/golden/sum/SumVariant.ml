type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)
  | HasMixed of int * string * double

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
       [ ( "tag", Json.Encode.string "HasMultipleInts" )
       ; ( "contents", Json.Encode.array [| Json.Encode.int y0 ; Json.Encode.string y1 ; Json.Encode.float y2 |] )
       ]

let decodeSumVariant (json : Js_json.t) :(sumVariant, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "HasNothing" ->
     Js_result.Ok HasNothing

  | "HasSingleInt" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (HasSingleInt v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: parse 'contents' " ^ message)
     )
  | "HasSingleTuple" ->
     (match Json.Decode.(field "contents" (pair int int) json) with
      | v -> Js_result.Ok (HasSingleTuple v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasSingleTuple: " ^ message)
     )

  | "HasMultipleInts" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.int v.(0) with
          | v0 ->
             (match Json.Decode.int v.(1) with
              | v1 -> Js_result.Ok (HasMultipleInts (v0,v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
         )
      | None -> Js_result.Error ("asdf")
     )

  | "HasMultipleTuples" ->
      (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.(pair int int) v.(0) with
          | v0 ->
             (match Json.Decode.(pair int int) v.(1) with
              | v1 -> Js_result.Ok (HasMultipleTuples (v0,v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleTuples: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleTuples: " ^ message)
         )
      | None -> Js_result.Error ("decodeSumVariant HasMultipleTuples expected an array.")
     )
  | err -> Js_result.Error ("decodeSumVariant: unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: " ^ message)
