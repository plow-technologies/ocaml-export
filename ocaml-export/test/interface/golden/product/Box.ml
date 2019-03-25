type 'a0 innerBox =
  { ibA : 'a0
  ; ibS : string
  ; ibX : int
  }

let encodeInnerBox encodeA0 x =
  Aeson.Encode.object_
    [ ( "ibA", encodeA0 x.ibA )
    ; ( "ibS", Aeson.Encode.string x.ibS )
    ; ( "ibX", Aeson.Encode.int x.ibX )
    ]

let decodeInnerBox decodeA0 json =
  match Aeson.Decode.
    { ibA = field "ibA" (fun a -> unwrapResult (decodeA0 a)) json
    ; ibS = field "ibS" string json
    ; ibX = field "ibX" int json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeInnerBox: " ^ message)

type outerBox =
  | OuterBox of (Person.person) innerBox

let encodeOuterBox x =
  match x with
  | OuterBox y0 ->
     (encodeInnerBox Person.encodePerson) y0

let decodeOuterBox json =
  match (Aeson.Decode.unwrapResult (decodeInnerBox Person.decodePerson json)) with
  | v -> Belt.Result.Ok (OuterBox v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeOuterBox: " ^ msg)

type auditAction =
  | Create
  | Delete
  | Update

let encodeAuditAction x =
  match x with
  | Create ->
     Aeson.Encode.string "Create"
  | Delete ->
     Aeson.Encode.string "Delete"
  | Update ->
     Aeson.Encode.string "Update"

let decodeAuditAction json =
  match Js_json.decodeString json with
  | Some "Create" -> Belt.Result.Ok Create
  | Some "Delete" -> Belt.Result.Ok Delete
  | Some "Update" -> Belt.Result.Ok Update
  | Some err -> Belt.Result.Error ("decodeAuditAction: unknown enumeration '" ^ err ^ "'.")
  | None -> Belt.Result.Error "decodeAuditAction: expected a top-level JSON string."

type 'a0 auditModel =
  { auditModel : 'a0
  ; originalId : string
  ; auditAction : auditAction
  ; editedBy : string
  ; editedOn : string
  }

let encodeAuditModel encodeA0 x =
  Aeson.Encode.object_
    [ ( "auditModel", encodeA0 x.auditModel )
    ; ( "originalId", Aeson.Encode.string x.originalId )
    ; ( "auditAction", encodeAuditAction x.auditAction )
    ; ( "editedBy", Aeson.Encode.string x.editedBy )
    ; ( "editedOn", Aeson.Encode.string x.editedOn )
    ]

let decodeAuditModel decodeA0 json =
  match Aeson.Decode.
    { auditModel = field "auditModel" (fun a -> unwrapResult (decodeA0 a)) json
    ; originalId = field "originalId" string json
    ; auditAction = field "auditAction" (fun a -> unwrapResult (decodeAuditAction a)) json
    ; editedBy = field "editedBy" string json
    ; editedOn = field "editedOn" string json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeAuditModel: " ^ message)

type user =
  { userIdent : string
  ; userPassword : (string) option
  ; userTimezone : string
  }

let encodeUser x =
  Aeson.Encode.object_
    [ ( "userIdent", Aeson.Encode.string x.userIdent )
    ; ( "userPassword", (Aeson.Encode.optional Aeson.Encode.string) x.userPassword )
    ; ( "userTimezone", Aeson.Encode.string x.userTimezone )
    ]

let decodeUser json =
  match Aeson.Decode.
    { userIdent = field "userIdent" string json
    ; userPassword = field "userPassword" (optional string) json
    ; userTimezone = field "userTimezone" string json
    }
  with
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeUser: " ^ message)

type userAudit =
  | UserAudit of (user) auditModel

let encodeUserAudit x =
  match x with
  | UserAudit y0 ->
     (encodeAuditModel encodeUser) y0

let decodeUserAudit json =
  match (Aeson.Decode.unwrapResult (decodeAuditModel decodeUser json)) with
  | v -> Belt.Result.Ok (UserAudit v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeUserAudit: " ^ msg)
