type class =
  { subject : string
  ; students : (Person.person) list
  ; professor : Person.person
  }

let encodeClass x =
  Aeson.Encode.object_
    [ ( "subject", Aeson.Encode.string x.subject )
    ; ( "students", (Aeson.Encode.list Person.encodePerson) x.students )
    ; ( "professor", Person.encodePerson x.professor )
    ]

let decodeClass json =
  match Aeson.Decode.
    { subject = field "subject" string json
    ; students = field "students" (list (fun a -> unwrapResult (Person.decodePerson a))) json
    ; professor = field "professor" (fun a -> unwrapResult (Person.decodePerson a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeClass: " ^ message)
