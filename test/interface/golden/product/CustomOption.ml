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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeCompany2: " ^ message)
