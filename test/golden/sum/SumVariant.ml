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
