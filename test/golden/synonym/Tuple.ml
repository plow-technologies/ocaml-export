type tuple =
  int * int

let encodeTuple (x : tuple) =
  (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b |]) x

