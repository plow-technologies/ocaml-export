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
  { id : int
  , name : option (string)
  };

let encodePerson (x : person) =>
  Json.Encode.object_
    [ ( "id", Json.Encode.int x.id )
    , ( "name", (fun a => Option.default Json.Encode.null (Option.map Json.Encode.string a)) x.name )
    ];

type company =
  { address : string
  , employees : list (person)
  };


let encodeCompany (x : company) =>
  Json.Encode.object_
    [ ( "address", Json.Encode.string x.address )
    , ( "employees", (Json.Encode.list encodePerson) x.employees )
    ];
