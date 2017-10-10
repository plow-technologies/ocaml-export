module Option = {
  let map (f: 'a => 'b) (o: option 'a) =>
    switch o {
    | None   => None
    | Some x => Some (f x)
    };

  let default (f: 'a) (o: option 'a) =>
    switch o {
    | None   => f
    | Some x => x
    };
};

type person =
  { id: int
  , name: option (string)
  };

let encodePerson (x: person) => 
  Json.Encode.object_
    [ ("id", Json.Encode.int x.id)
    , ("name", (fun xx => Option.default Json.Encode.null (Option.map Json.Encode.string xx)) x.name)
    ];

type rect =
  { point1: (int,int)
  , point2: (int,int)
  , plot: bool
  };

let encodeRect (x: rect) => 
  Json.Encode.object_
    [ ("point1", (fun (a,b) => Json.Encode.array [| Json.Encode.int a, Json.Encode.int b |]) x.point1)
    , ("point2", (fun (a,b) => Json.Encode.array [| Json.Encode.int a, Json.Encode.int b |]) x.point1)  
    , ("plot", (fun a => Json.Encode.boolean (Js.Boolean.to_js_boolean a)) x.plot)
    ];

type point =
  { x: int
  , y: int
  };

let encodePoint (x: point) => 
  Json.Encode.object_
    [ ("x", Json.Encode.int x.x)
    , ("y", Json.Encode.int x.y)
    ];

type thing =
  { x: Js_dict.t Js_json.t
  , y: Js_dict.t int
  };

type thing2 =
  { x: Js_dict.t int };

let json : Js_dict.t Js_json.t = Js_dict.empty();
Js_dict.set json "firstName" (Js_json.string "hi");

let json2 : Js_dict.t string = Js_dict.empty();
Js_dict.set json2 "firstName" "hi";
Js_dict.set json2 "lastName" "bye";
/* 
doesn't work, must be a string
Js_dict.set json2 1 "bye";
*/

/*
let json : Js_dict.t string = Js_dict.empty();
Js_dict.set json "firstName" (Js_json.string "hi");
*/
/*
let z : thing2 = {x: {"hi" : 1}}
*/
