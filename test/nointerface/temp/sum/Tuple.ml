(int * int)

(fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |])

Json.Decode.(pair int int json)
