type tuple =
  int * int

let encodeTuple (x : tuple) =
  (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b |]) x

let decodeTuple (json : Js_json.t) :(tuple, string) Js_result.t =
  Json.Decode.(pair int int json)
