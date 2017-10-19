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

let decodePerson (json : Js_json.t) :person option =
  match Json.Decode.
    { id = field "id" int json
    ; name = field "name" string json
    }
  with
  | v -> Some v
  | exception Json.Decode.DecodeError _ -> None
(*
let decodePersonSafe (json : Js_json.t) :person option =
  Json.Decode.(
    match field "id" int json with
    | f0 ->
       (match field "name" string json with
        | f1 -> Some {id = f0 ; name = f1}
        | exception DecodeError _ -> None
       )
    | exception DecodeError _ -> None
  )


let decodePersonSafe (json : Js_json.t) :person option =
  match decodePerson json with
  | exception Json.Decode.DecodeError _ -> None
  | v -> Some v
  *)
(*
  match Json.Decode.(field "x" int json) with
  | x ->
    Js.log x
  | exception Json.Decode.DecodeError msg ->
Js.log ("Error:" ^ msg)

match decode json with
  | exception DecodeError _ -> None
| v -> Some v
*)

  
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

(*
let x = Json.Encode.array [| Json.Encode.int 1 ; Json.Encode.string "asdf" |]
let y = [1 ; 2 ]
let z = [ Json.Encode.int 1 ; Json.Encode.int 2 ; Json.Encode.string "asdf" ]
 *)
type tuple =
  int * int

let encodeTuple (x : tuple) =
  (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b |]) x

type withTuple =
  | WithTuple of (int * int)

let encodeWithTuple (x : withTuple) =
  match x with
  | WithTuple y0 ->
     (fun (a,b) -> Json.Encode.array [| Json.Encode.int a ; Json.Encode.int b  |]) y0

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

let encodeSuit (x : suit) =
  match x with
  | Clubs ->
     Json.Encode.string "Clubs"
  | Diamonds ->
     Json.Encode.string "Diamonds"
  | Hearts ->
     Json.Encode.string "Hearts"
  | Spades ->
     Json.Encode.string "Spades"
  
type card =
  { suit  : suit
  ; value : int
  }

let encodeCard (x : card) =
  Json.Encode.object_
    [ ( "suit", encodeSuit x.suit )
    ; ( "value", Json.Encode.int x.value )
    ]

(*work around for the fact that ocaml does not support sum of product that has named accessors *)
  
type coo =
  | CooX of card
  | CooY of int 

let encodeCoo (x : coo) =
  match x with
  | CooX y0 ->
     (match (Js.Json.decodeObject (encodeCard y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "CooX");
         Js.Json.object_ dict;
      | None ->
         Json.Encode.object_ []
     )
  | CooY y0 ->
     Json.Encode.object_
       [ ( "tag", Json.Encode.string "CooY" )
       ; ( "contents", Json.Encode.int y0 )
       ]

type sumWithRecordA1 =
  { a1 : int
  }

type sumWithRecordB2 =
  { b2 : string
  ; b3 : int
  }

type sumWithRecord =
  | A1 of sumWithRecordA1
  | B2 of sumWithRecordB2

let encodeSumWithRecordA1 (x : sumWithRecordA1) :Js_json.t =
  Json.Encode.object_
    [ ( "a1", Json.Encode.int x.a1 )
    ]

let encodeSumWithRecordB2 (x : sumWithRecordB2) :Js_json.t =
  Json.Encode.object_
    [ ( "b2", Json.Encode.string x.b2 )
    ; ( "b3", Json.Encode.int x.b3 )
    ]


let encodeSumWithRecord (x : sumWithRecord) :Js_json.t =
  match x with
  | A1 y0 ->
     (match (Js.Json.decodeObject (encodeSumWithRecordA1 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "A1");
         Js.Json.object_ dict
      | None ->
         Json.Encode.object_ []
     )
  | B2 y0 ->
     (match (Js.Json.decodeObject (encodeSumWithRecordB2 y0)) with
      | Some dict ->
         Js.Dict.set dict "tag" (Js.Json.string "B2");
         Js.Json.object_ dict
      | None ->
         Json.Encode.object_ []
     )

type 'a0 holdone =
  { id : int
  ; thing1 : 'a0
  }

type holdstring = string holdone

let encodeHoldone (parseA0 : 'a0 -> Js_json.t) (x : 'a0 holdone) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "thing1", parseA0 x.thing1 )
    ]

type ('a0, 'a1) twoTypeParameters =
  { id : int
  ; first : 'a0
  ; second : 'a1
  }

let encodeTwoTypeParameters (type a0) (type a1) (parseA0 : a0 -> Js_json.t) (parseA1 : a1 -> Js_json.t) (x : (a0, a1) twoTypeParameters) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "first", parseA0 x.first )
    ; ( "second", parseA1 x.second )
    ]  

type ('a0, 'a1) holdtwo =
  { id : int
  ; thing1 : 'a0
  ; thing2 : 'a1
  }

(* let encodeHoldtwo : 'a0 'a1. (parseA0 : 'a0 -> Js_json.t) (parseA1 : 'a1 -> Js_json.t) (x : ('a0, 'a1) holdtwo) :Js_json.t = *)
let encodeHoldtwo (type a0) (type a1) (parseA0 : a0 -> Js_json.t) (parseA1 : a1 -> Js_json.t) (x : (a0, a1) holdtwo) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "thing1", parseA0 x.thing1 )
    ; ( "thing2", parseA1 x.thing2 )
    ]  

let x = encodeHoldtwo Json.Encode.string Json.Encode.int {id = 1; thing1 = "Hi"; thing2 = 12}
(* let y = encodeHoldtwo Json.Encode.string Json.Encode.int {id = 1; thing1 = 12; thing2 = "Hi"} *)
      
type holdstringandint = (string, int) holdtwo

type 'a0 oneTypeParameter =
  { otpId : int
  ; otpFirst : 'a0
  }

let encodeOneTypeParameter (type a0) (parseA0 : a0 -> Js_json.t) (x : a0 oneTypeParameter) :Js_json.t =
  Json.Encode.object_
    [ ( "otpId", Json.Encode.int x.otpId )
    ; ( "otpFirst", parseA0 x.otpFirst )
    ]


    (* let decode (x : card) :Js_json.t option = *)
(*


  let parseCompanyJson json :company =>
  Json.Decode.{
    address: field "address" string json,
    city:    field "city" string json,
    delete:  field "delete" int json,
    gid:     optional (field "gid" string) json,
    name:    field "name" string json,
    oid:     optional (field "oid" int) json,
    refId:   field "refId" int json,
    state:   field "state" string json,
    zip:     field "zip" string json
  };

let parseCompanyEntityJson json :companyEntity =>
  Json.Decode.{
    key:   field "key" string json,
    value: field "value" parseCompanyJson json
  };

let parseCompanyEntitiesJson json :list companyEntity =>
  Json.Decode.list parseCompanyEntityJson json;
    *)

let () = Js.log "The End";

let p = Js.Json.parseExn {| {"id": 1,"name":"James"} |} in let pp = decodePerson p in Js.log pp;


(*
type person =
  { id: int
  ; name: string
  }

let encodePerson (x: person) =
  Json.Encode.object_
    [ ("id", Json.Encode.int x.id)
    ; ("name", Json.Encode.string x.name)
    ]

let decodePerson (json : Js_json.t) :person =
  Json.Decode.
    { id = field "id" int json
    ; name = field "name" string json
    }

let decodePersonSafe (json : Js_json.t) :person option =
  match decodePerson json with
  | exception Invalid_argument _ -> None
  | v -> Some v
         *)
