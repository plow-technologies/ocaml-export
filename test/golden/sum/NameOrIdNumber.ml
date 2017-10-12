type nameOrIdNumber =
  | Name of string
  | IdNumber of int

let encodeNameOrIdNumber (x : nameOrIdNumber) =
  match x with
  | Name y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "Name" )
       ; ( "contents", Json.Encode.string y0 )
       ]
  | IdNumber y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "IdNumber" )
       ; ( "contents", Json.Encode.int y0 )
       ]
