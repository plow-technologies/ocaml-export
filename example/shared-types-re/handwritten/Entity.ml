type ('key, 'value) entity =
  { entityKey   : 'key
  ; entityValue : 'value
  }

let encodeEntity encodeKey encodeValue entity =
  Aeson.Encode.object_
    [ ( "key", encodeKey entity.entityKey )
    ; ( "value", encodeValue entity.entityValue )
    ]

let decodeEntity decodeKey decodeValue json =
  match Aeson.Decode.
    { entityKey   = field "key" (fun a -> unwrapResult (decodeKey a)) json
    ; entityValue = field "value" (fun a -> unwrapResult (decodeValue a)) json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeEntity: " ^ message)
