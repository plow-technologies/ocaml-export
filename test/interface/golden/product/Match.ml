external toJsObject : 'a Js.Dict.t -> < .. > Js.t = "%identity"

(*
let p  : Person.person = { id = 0; name = Some "James" ; created = Js_date.fromFloat (Js_date.now ()) } in
let p2 : Person.person = { id = 1; name = Some "James" ; created = Js_date.fromFloat (Js_date.now ()) } in
Js.log (p);
Js.log ((Js_result.Ok p) = Person.decodePerson (Person.encodePerson p));
Js.log (p = p2);
assert (1 = 1);
assert (1 = 0);
 *)

(*
let () =
  (* read [package.json] file *)
  Node.Fs.readFileAsUtf8Sync "person.json"
  |> Js.Json.parseExn
  |> Person.decodePerson
  |> Js.log
 *)
(*
Bs_fetch.fetch q
        |> Js.Promise.then_ Bs_fetch.Response.json
        |> Js.Promise.then_ (fun json =>
           Js.Promise.resolve (f ((parseSiteEntitiesJson json)))
);
 *)
(*
let headers = Bs_fetch.HeadersInit.make [%bs.obj { "Content-Type" = "application/json" } ] in

curl -i -d '{"id":0,"name":"James","created":"2017-11-07T07:00:46.316346Z"}' -H 'Content-type: application/json' -X POST http://localhost:8081/person
 *)
                                            (*
    let headers = Bs_node_fetch.HeadersInit.makeWithArray [| ( "Content-Type", "application/json" ) |] in
     *)

let () =
let p  : Person.person = { id = 0; name = Some "James" ; created = Js_date.fromFloat (Js_date.now ()) } in


let headerDict = toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")]) in
let hs = Js_dict.fromList [("Content-Type" , "application/json")] in
let headers = Bs_node_fetch.HeadersInit.make headerDict in

let fetchthing () =
  Js.log "fetchthing";
  let reqInit = 
    Bs_node_fetch.RequestInit.make
      ~method_:Bs_node_fetch.Post
      ~mode:Bs_node_fetch.CORS
      ~body:(Bs_node_fetch.BodyInit.make (Js.Json.stringify (Person.encodePerson p)))
      ~headers:headers
      () in

  Js.Promise.(
    Bs_node_fetch.fetchWithInit "http://localhost:8081/person" reqInit
    |> then_ (fun response -> (Bs_node_fetch.Response.text response)
    |> then_ (fun text -> Js.log "text received" ; Js.log (Bs_node_fetch.Response.statusText response) ;  Js.log text |> resolve))
  );

in fetchthing ();
Js.log (Js.Json.stringify (Person.encodePerson p));
Js.log (headers);
[%%bs.raw{|
  console.log ("hey");
setTimeout(function() {
    console.log('Blah blah blah blah extra-blah');
}, 3000);
          |}]




(*
open Bs_node_fetch
let _ =
  let headers = Bs_node_fetch.HeadersInit.makeWithArray [| ( "Content-Type", "application/json" ) |] in
  Js.Promise.(
    fetchWithInit "https://dog.ceo/api/breeds/list/all" (RequestInit.make ~method_:Post ~headers:headers ())
    |> then_ Response.text
    |> then_ (fun text -> print_endline text |> resolve)
)
 *)
   (*
Js.Promise.(
  Bs_fetch.fetch "http://localhost:3000/"
  |> then_ Bs_fetch.Response.text
  |> then_ (fun text -> Js.log text |> resolve)
)
 *)

(*
let fetchLocations (companyIds : list int) (siteIds : list int) f => {
  let reqInit = 
    Bs_fetch.RequestInit.make
      method_::Bs_fetch.Post
      mode::Bs_fetch.CORS
      body::(Bs_fetch.BodyInit.make (Js.Json.stringify (encodeLocationLookupId {getLocationLookupId: None, getCompanyLookupList: companyIds, getSiteLookupList: siteIds})))
      headers::headers
      ();
  Bs_fetch.fetchWithInit onpingLocationApi reqInit
    |> Js.Promise.then_ Bs_fetch.Response.json
    |> Js.Promise.then_ (fun json =>
       Js.Promise.resolve (f ((parseLocationEntitiesJson json)))
    );  
}


  let testServer =  in
  Bs_fetch.fetch testServer
  |> Js.Promise.then_ Bs_fetch.Response.json
  |> Js.Promise.then_ (fun json ->
                             Js.Promise.resolve (Js.log (Person.decodePerson json)));
 *)
(*
let dict = Js_dict.empty () in 
Js_dict.set dict "name" (Js_json.string "John Doe");
Js_dict.set dict "age" (Js_json.numberOfInt 30);
Js_dict.set dict "likes"
  (Js_json.stringArray [|"bucklescript";"ocaml";"js"|]);

Js.log @@ Js_json.stringify (Js_json.object_ dict) 
 *)
