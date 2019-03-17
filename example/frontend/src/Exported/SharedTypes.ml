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


type key =
  | Key of int

let encodeKey x =
  match x with
  | Key y0 ->
     Aeson.Encode.int y0

let decodeKey json =
  match Aeson.Decode.int json with
  | v -> Belt.Result.Ok (Key v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeKey: " ^ msg)

type todoId =
  | TodoId of key

let encodeTodoId x =
  match x with
  | TodoId y0 ->
     encodeKey y0

let decodeTodoId json =
  match (Aeson.Decode.unwrapResult (decodeKey json)) with
  | v -> Belt.Result.Ok (TodoId v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeTodoId: " ^ msg)

type userId =
  | UserId of key

let encodeUserId x =
  match x with
  | UserId y0 ->
     encodeKey y0

let decodeUserId json =
  match (Aeson.Decode.unwrapResult (decodeKey json)) with
  | v -> Belt.Result.Ok (UserId v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeUserId: " ^ msg)

type username =
  | Username of string

let encodeUsername x =
  match x with
  | Username y0 ->
     Aeson.Encode.string y0

let decodeUsername json =
  match Aeson.Decode.string json with
  | v -> Belt.Result.Ok (Username v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeUsername: " ^ msg)

type todo =
  { description : string
  ; completed : bool
  }

let encodeTodo x =
  Aeson.Encode.object_
    [ ( "description", Aeson.Encode.string x.description )
    ; ( "completed", Aeson.Encode.bool x.completed )
    ]

let decodeTodo json =
  match Aeson.Decode.
    { description = field "description" string json
    ; completed = field "completed" bool json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeTodo: " ^ message)

type user =
  { username : username
  ; password : string
  }

let encodeUser x =
  Aeson.Encode.object_
    [ ( "username", encodeUsername x.username )
    ; ( "password", Aeson.Encode.string x.password )
    ]

let decodeUser json =
  match Aeson.Decode.
    { username = field "username" (fun a -> unwrapResult (decodeUsername a)) json
    ; password = field "password" string json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeUser: " ^ message)
