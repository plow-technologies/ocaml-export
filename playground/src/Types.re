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

