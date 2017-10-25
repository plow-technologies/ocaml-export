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

(*
module Result = struct
  
end
 *)
              
type person =
  { id : int
  ; name : (string) option
  }

let bob = { id = 1; name = Some "Bob"}
let billy = { bob with name = Some "Billy"}   
let encodePerson (x : person) :Js_json.t =
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    ; ( "name", (fun a -> Option.default Json.Encode.null (Option.map Json.Encode.string a)) x.name )
    ]

let decodePerson (json : Js_json.t) :person option =
  match Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Some v
  | exception Json.Decode.DecodeError _ -> None

(*                                          
let decodePerson (json : Js_json.t) :person =
  Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    } 
 *)
let decodePersonResult (json : Js_json.t) :(person, string) Js_result.t =
  match Json.Decode.
    { id = field "id" int json
    ; name = optional (field "name" string) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodePersonResult: " ^ message)


type company =
  { address : string
  ; employees : (person) list
  }

let encodeCompany (x : company) :Js_json.t =
  Json.Encode.object_
    [ ( "address", Json.Encode.string x.address )
    ; ( "employees", (Json.Encode.list encodePerson) x.employees )
    ]

let unwrapOption (oa : 'a option) :'a =
  match oa with
  | Some a -> a
  | None -> raise @@ Json.Decode.DecodeError ""

let unwrapResult (type a) (r: (a, string) Js_result.t) :a =
  match r with
  | Js_result.Ok a -> a
  | Js_result.Error message -> raise @@ Json.Decode.DecodeError message
  
let decodeCompany (json : Js_json.t) :company option =
  match Json.Decode.
    { address = field "address" string json
    ; employees = field "employees" (list (fun a -> unwrapOption (decodePerson a))) json
    }
  with
  | v -> Some v
  | exception Json.Decode.DecodeError _ -> None

(*
list (fun a -> unwrapResult (field "employees" decodePersonResult a)) json
 *)                                         
let decodeCompanyResult (json : Js_json.t) :(company, string) Js_result.t =
  match Json.Decode.
    { address = field "address" string json
    ; employees = field "employees" (list (fun a -> unwrapResult (decodePersonResult a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeCompanyResult: " ^ message)

  
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
       [ ("tag", Json.Encode.string "IdNumber")
       ; ( "contents", Json.Encode.int a )
       ]

let decodeNameOrIdNumber (json : Js_json.t) :(nameOrIdNumber, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "Name" ->
     (match Json.Decode.(field "contents" string json) with
      | v -> Js_result.Ok (Name v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeNameOrIdNumber: parse 'contents' " ^ message)
     )
  | "IdNumber" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (IdNumber v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeNameOrIdNumber: parse 'contents' " ^ message)
     )
  | err -> Js_result.Error ("decodeCompany: unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeNameOrIdNumber: " ^ message)


(*
unnamed products of more than one value are in array
match Js.Json.decodeArray json with
| Some a
| None -> Js_result.Error ""
*)
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


external _unsafeCreateUninitializedArray : int -> 'a array = "Array" [@@bs.new]
                                                           
let arrayOfUndecodedValues json = 
  if Js.Array.isArray json then begin
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    let length = Js.Array.length source in
    let target = _unsafeCreateUninitializedArray length in
    for i = 0 to length - 1 do
      let value = (Array.unsafe_get source i) in
      Array.unsafe_set target i value;
    done;
    target
  end
  else
    raise @@ Json.Decode.DecodeError ("Expected array, got " ^ Js.Json.stringify json)

let decodeSumVariant (json : Js_json.t) :(sumVariant, string) Js_result.t =
  match Json.Decode.(field "tag" string json) with
  | "HasNothing" -> Js_result.Ok HasNothing
  | "HasSingleInt" ->
     (match Json.Decode.(field "contents" int json) with
      | v -> Js_result.Ok (HasSingleInt v)
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: parse 'contents' " ^ message)
     )
  | "HasSingleTuple" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.int v.(0) with
          | v0 ->
             (match Json.Decode.int v.(1) with
              | v1 -> Js_result.Ok (HasSingleTuple (v0,v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasSingleTuple: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasSingleTuple: " ^ message)
         )
      | None -> Js_result.Error ("asdf")
     )
  | "HasMultipleInts" ->
     (match Json.Decode.(field "contents" Js.Json.decodeArray json) with
      | Some v ->
         (match Json.Decode.int v.(0) with
          | v0 ->
             (match Json.Decode.int v.(1) with
              | v1 -> Js_result.Ok (HasMultipleInts (v0,v1))
              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
         )
      | None -> Js_result.Error ("asdf")
     )
  | "HasMultipleTuples" ->
     (match Json.Decode.(field "contents" arrayOfUndecodedValues json) with
      | v ->
         (match arrayOfUndecodedValues v.(0) with
          | v0 ->
             (match arrayOfUndecodedValues v.(1) with
              | v1 ->
                 (match Json.Decode.int v0.(0) with
                  | v2 ->
                     (match Json.Decode.int v0.(1) with
                      | v3 ->
                         (match Json.Decode.int v1.(0) with
                          | v4 ->
                             (match Json.Decode.int v1.(1) with
                              | v5 -> Js_result.Ok (HasMultipleTuples ((v2,v3), (v4,v5)))
                              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
                             )
                          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
                         )
                      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
                     )
                  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
                 )
              | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
             )
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
         )
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant HasMultipleInts: " ^ message)
     )
  | err -> Js_result.Error ("decodeCompany: unknown tag value found '" ^ err ^ "'.")
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeSumVariant: " ^ message)


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


let decodeWithTuple2 (json : Js_json.t) :(withTuple, string) Js_result.t =
  match Json.Decode.(tuple2 int int json) with
  | v -> Js_result.Ok (WithTuple v)
  | exception Json.Decode.DecodeError msg -> Js_result.Error msg


    
let decodeWithTuple (json : Js_json.t) :(withTuple, string) Js_result.t =
  match Js.Json.decodeArray json with
  | Some v when Js_array.length v == 2 ->
     (match Json.Decode.int v.(0) with
      | v0 ->
         (match Json.Decode.int v.(1) with
          | v1 -> Js_result.Ok (WithTuple (v0,v1))
          | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeWithTuple HasMultipleInts: " ^ message)
         )
      | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeWithTuple HasMultipleInts: " ^ message)
     )
  | _ -> Js_result.Error ("decodeWithTuple: unable to decodeArray.")

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

let decodeSuit (json : Js_json.t) :(suit, string) Js_result.t =
  match Js_json.decodeString json with
  | Some "Clubs" -> Js_result.Ok Clubs
  | Some "Diamonds" -> Js_result.Ok Diamonds
  | Some "Hearts" -> Js_result.Ok Hearts
  | Some "Spades" -> Js_result.Ok Spades
  | Some x -> Js_result.Error ("decodeSuit: unknown enumeration '" ^ x ^ "'")
  | None -> Js_result.Error "decodeSuit: expected string"
    
type card =
  { cardSuit  : suit
  ; cardValue : int
  }

let encodeCard (x : card) =
  Json.Encode.object_
    [ ( "cardSuit", encodeSuit x.cardSuit )
    ; ( "cardValue", Json.Encode.int x.cardValue )
    ]

let decodeCard (json : Js_json.t) :(card, string) Js_result.t =
  match Json.Decode.
    { cardSuit = field "cardSuit" (fun a -> unwrapResult (decodeSuit a)) json
    ; cardValue = field "cardValue" int json
    }
  with
  | v -> Js_result.Ok v
  | exception Json.Decode.DecodeError message -> Js_result.Error ("decodeCard: " ^ message)

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


let safeGet (type a) (xs : a Js_array.t) (x : int) :(a,string) Js_result.t =
  match xs.(x) with
  | value -> Js_result.Ok value
  | exception Js.Exn.Error e ->
    match Js.Exn.message e with
    | Some message -> Js_result.Error message
    | None -> Js_result.Error "An unknown error occurred"

let safeGet2 (type a) (xs : a Js_array.t) (i :int) :a Js_option.t =
  if i >= 0 && i < Js_array.length xs + 1 then Some xs.(i) else None



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

(* compare fail results *)
let p = Js.Json.parseExn {| {"id": 1,"name":"James"} |} in let pp = decodePerson p in Js.log pp;
let p = Js.Json.parseExn {| {"id": 1,"name":"James"} |} in let pp = decodePerson p in Js.log pp;

let p = Js.Json.parseExn {| {"id": 1,"name":"James"} |} in let pp = decodePersonResult p in Js.log pp;
let p = Js.Json.parseExn {| {"id": 1,"name":"James"} |} in let pp = decodePersonResult p in Js.log pp;

let p = Js.Json.parseExn {| {"address": "OK City", "employees": [ {"id": 1,"name":"James"} ]} |} in let pp = decodeCompany p in Js.log pp;
let p = Js.Json.parseExn {| {"address": "OK City", "employees": [ {"id": 1,"name":"James"} ]} |} in let pp = decodeCompanyResult p in Js.log pp;



let p = Js.Json.parseExn {| {"cardSuit": "Diamonds", "cardValue": 1} |} in let pp = decodeCard p in Js.log pp;

let p = Js.Json.parseExn {| {"cardSuit": "Diamond", "cardValue": 10} |} in let pp = decodeCard p in Js.log pp;

let p = Js.Json.parseExn {| {"tag": "IdNumber", "contents": 1} |} in let pp = decodeNameOrIdNumber p in Js.log pp;

let p = Js.Json.parseExn {| {"tag": "IdNumber", "contents": "hello"} |} in let pp = decodeNameOrIdNumber p in Js.log pp;

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

(* length *)
let x = [| 1 ; 2 ; 3 |] in Js.log (safeGet2 x 2);

let zzz = HasMultipleInts (1, 2) in Js.log zzz;
let zzz = HasMultipleTuples ((1, 2), (3,4)) in Js.log zzz;
                                                                                   
let yy = Js.Json.parseExn {| {"tag" : "HasMultipleInts", "contents" : [1, 2]} |} in let pp = decodeSumVariant yy in Js.log pp;
let yy = Js.Json.parseExn {| {"tag" : "HasMultipleTuples", "contents" : [[1, 2],[3,4]]} |} in let pp = decodeSumVariant yy in Js.log pp;
let yy = Js.Json.parseExn {| {"tag" : "HasSingleTuple", "contents" : [1, 2]} |} in let pp = decodeSumVariant yy in Js.log pp;
let yy = Js.Json.parseExn {| {"tag" : "HasSingleTuple", "contents" : [1, "asdf"]} |} in let pp = decodeSumVariant yy in Js.log pp;

let zzz = Js.Json.parseExn "[1, 2, \"c\"]" in let pp = arrayOfUndecodedValues zzz in Js.log pp;
