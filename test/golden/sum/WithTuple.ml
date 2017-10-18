type withTuple =
  | WithTuple of (int * int)

let encodeWithTuple (x : withTuple) :Js_json.t =
  match x with
  | WithTuple y0 ->
     (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0
