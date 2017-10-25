type withTuple =
  | WithTuple of (int * int)

let encodeWithTuple (x : withTuple) :Js_json.t =
  match x with
  | WithTuple y0 ->
     (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0

let decodeWithTuple (json : Js_json.t) :(withTuple, string) Js_result.t =
  match Js.json.decodeArray json with
  | Some v when Js_array.length == 2 ->
     (match Json.Decode.int v.(0) with
      | Some v0 ->
         (match Json.Decode.int v.(1) with
          | Some v1 ->
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeWithTuple HasMultipleInts: " ^ message)
         )
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeWithTuple HasMultipleInts: " ^ message)
     )
  | _ -> Js_result.Error ("decodeWithTuple: unable to decodeArray.")
