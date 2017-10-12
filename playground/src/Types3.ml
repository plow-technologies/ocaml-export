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


    (*  | Name (a) ->
     (Json.Encode.object_
       [ ( "tag", Json.Encode.string "Name" )
       , ( "contents", Json.Encode.string a )
       ])
  | IdNumber (a) ->
     (Json.Encode.object_
       [ ( "tag", Json.Encode.string "IdNumber" )
       , ( "contents", Json.Encode.int a )
     ]) *)
         
