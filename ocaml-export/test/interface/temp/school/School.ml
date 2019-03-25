type school =
  { schoolName : string
  ; classes : (Class.class) list
  ; administrator : Person.person
  }

let encodeSchool x =
  Aeson.Encode.object_
    [ ( "schoolName", Aeson.Encode.string x.schoolName )
    ; ( "classes", (Aeson.Encode.list Class.encodeClass) x.classes )
    ; ( "administrator", Person.encodePerson x.administrator )
    ]

let decodeSchool json =
  match Aeson.Decode.
    { schoolName = field "schoolName" string json
    ; classes = field "classes" (list (fun a -> unwrapResult (Class.decodeClass a))) json
    ; administrator = field "administrator" (fun a -> unwrapResult (Person.decodePerson a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSchool: " ^ message)
