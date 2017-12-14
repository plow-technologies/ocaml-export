type company2 =
  { address2 : string
  ; boss : (Person.person) option
  }

let encodeCompany2 x =
  Aeson.Encode.object_
    [ ( "address2", Aeson.Encode.string x.address2 )
    ; ( "boss", Aeson.Encode.optional Person.encodePerson x.boss )
    ]

let decodeCompany2 json =
  match Aeson.Decode.
    { address2 = field "address2" string json
    ; boss = optional (field "boss" (fun a -> unwrapResult (Person.decodePerson a))) json
    }
  with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeCompany2: " ^ message)
