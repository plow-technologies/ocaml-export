type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)

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

let decodeSumVariant (json : Js_json.t) :(sumVariant, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "HasNothing" -> Js_result.Ok "HasNothing"
  | "HasSingleInt" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (HasSingleInt v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: parse 'contents' " ^ message)
     )
  | "HasMultipleInts" ->
     (match Json.Decode.(field "contents" array json) with
      | v ->
         (if Js_array.length v == 2
          then Js_result.Ok (HasMultipleInts (v.[0]) (v.[1]))
          else Js_result.Error "Too short")
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: parse 'contents' " ^ message)
     )
  | err -> Js_result.Error ("decodeCompany: unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeNameOrIdNumber: " ^ message)
