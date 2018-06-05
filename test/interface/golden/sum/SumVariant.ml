type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)
  | HasMixed of int * string * float

let encodeSumVariant x =
  match x with
  | HasNothing ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasNothing" )
       ]
  | HasSingleInt y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasSingleInt" )
       ; ( "contents", Aeson.Encode.int y0 )
       ]
  | HasSingleTuple y0 ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasSingleTuple" )
       ; ( "contents", (Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int) y0 )
       ]
  | HasMultipleInts (y0,y1) ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasMultipleInts" )
       ; ( "contents", Aeson.Encode.array [| Aeson.Encode.int y0 ; Aeson.Encode.int y1 |] )
       ]
  | HasMultipleTuples (y0,y1) ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasMultipleTuples" )
       ; ( "contents", Aeson.Encode.array [| (Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int) y0 ; (Aeson.Encode.pair Aeson.Encode.int Aeson.Encode.int) y1 |] )
       ]
  | HasMixed (y0,y1,y2) ->
     Aeson.Encode.object_
       [ ( "tag", Aeson.Encode.string "HasMixed" )
       ; ( "contents", Aeson.Encode.array [| Aeson.Encode.int y0 ; Aeson.Encode.string y1 ; Aeson.Encode.float y2 |] )
       ]

let decodeSumVariant json =
  match Aeson.Decode.(field "tag" string json) with
  | "HasNothing" ->
     Belt.Result.Ok HasNothing

  | "HasSingleInt" ->
     (match Aeson.Decode.(field "contents" int json) with
      | v -> Belt.Result.Ok (HasSingleInt v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasSingleInt: " ^ message)
     )

  | "HasSingleTuple" ->
     (match Aeson.Decode.(field "contents" (pair int int) json) with
      | v -> Belt.Result.Ok (HasSingleTuple v)
      | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasSingleTuple: " ^ message)
     )
  | "HasMultipleInts" ->
     (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Aeson.Decode.int v.(0) with
          | v0 ->
             (match Aeson.Decode.int v.(1) with
              | v1 ->
                 Belt.Result.Ok (HasMultipleInts (v0, v1))
              | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMultipleInts: " ^ message)
             )
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMultipleInts: " ^ message)
         )
      | None -> Belt.Result.Error ("HasMultipleInts expected an array.")
     )

  | "HasMultipleTuples" ->
     (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Aeson.Decode.(pair int int) v.(0) with
          | v0 ->
             (match Aeson.Decode.(pair int int) v.(1) with
              | v1 ->
                 Belt.Result.Ok (HasMultipleTuples (v0, v1))
              | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMultipleTuples: " ^ message)
             )
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMultipleTuples: " ^ message)
         )
      | None -> Belt.Result.Error ("HasMultipleTuples expected an array.")
     )

  | "HasMixed" ->
     (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Aeson.Decode.int v.(0) with
          | v0 ->
             (match Aeson.Decode.string v.(1) with
              | v1 ->
                 (match Aeson.Decode.float v.(2) with
                  | v2 ->
                     Belt.Result.Ok (HasMixed (v0, v1, v2))
                  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMixed: " ^ message)
                 )
              | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMixed: " ^ message)
             )
          | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("HasMixed: " ^ message)
         )
      | None -> Belt.Result.Error ("HasMixed expected an array.")
     )
  | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message
