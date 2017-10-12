Js.log "Hello"

module Option = struct
  let map (f: 'a -> 'b) (o: 'a option) =
    match o with
    | None   -> None
    | Some x -> Some (f x)
    
  let default (f: 'a) (o: 'a option) =
    match o with
    | None   -> f
    | Some x -> x    
end

     
type person =
  { id: int
  ; name: string
  }

let encodePerson (x: person) =
  Json.Encode.object_
    [ ("id", Json.Encode.int x.id)
    ; ("name", Json.Encode.string x.name)
    ]

type person2 =
  { id: int
  ; name: string option
  }

let encodePerson2 (x: person2) =
  Json.Encode.object_
    [ ("id", Json.Encode.int x.id)
    ; ("name", (fun xx -> Option.default Json.Encode.null (Option.map Json.Encode.string xx)) x.name)
    ]

type thing =
  | First
  | Second

type ttuple = int * string

type thing2 =
  | ABC of int
  | DEF of int * string

type company =
  { address : string
  ; employees : (person) list
  }


let encodeCompany (x : company) =
  Json.Encode.object_
    [ ( "address", Json.Encode.string x.address )
    ; ( "employees", (Json.Encode.list encodePerson) x.employees )
    ]
         
type onOrOff =
  | On
  | Off

let encodeOnOrOff (x : onOrOff) =
  match x with
  | On ->
     Json.Encode.string "On"
  | Off ->
     Json.Encode.string "Off"


type nameOrIdNumber =
  | Name of string
  | IdNumber of int

let encodeNameOrIdNumber (x : nameOrIdNumber) =
  match x with
  | Name a ->
     Json.Encode.object_
       [ ("tag", Json.Encode.string "Name")
       ; ( "contents", Json.Encode.string a )
       ]
  | IdNumber a ->
     Json.Encode.object_
       [ ("tag", Json.Encode.string "Name")
       ; ( "contents", Json.Encode.int a )
       ]

type sumVariant =
  | HasNothing
  | HasSingleInt of int
  | HasSingleTuple of (int * int)
  | HasMultipleInts of int * int
  | HasMultipleTuples of (int * int) * (int * int)

let encodeSumVariant (x : sumVariant) =
  match x with
  | HasNothing ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasNothing" )
       ]
  | HasSingleInt (y0) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasSingleInt" )
       ; ( "contents", Json.Encode.int y0 )
       ]
  | HasSingleTuple (y0) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasSingleTuple" )
       ; ( "contents", (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0 )
       ]
  | HasMultipleInts (y0,y1) ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "HasMultipleInts" )
       ; ( "contents", Json.Encode.array [|Json.Encode.int y0 ; Json.Encode.int y1  |] )
       ]
  | HasMultipleTuples (y0,y1) ->
     Json.Encode.object_
       [ ("tag", Json.Encode.string "HasMultipleInts")
       ; ( "contents", Json.Encode.array [| (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0 ; (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y1 |] )
       ]

let x = Json.Encode.array [| Json.Encode.int 1 ; Json.Encode.string "asdf" |]
let y = [1 ; 2 ]
let z = [ Json.Encode.int 1 ; Json.Encode.int 2 ; Json.Encode.string "asdf" ]
