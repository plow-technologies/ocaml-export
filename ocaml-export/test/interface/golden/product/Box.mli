type 'a0 innerBox =
  { ibA : 'a0
  ; ibS : string
  ; ibX : int
  }

val encodeInnerBox : ('a0 -> Js_json.t) -> 'a0 innerBox -> Js_json.t

val decodeInnerBox : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 innerBox, string) Belt.Result.t

type outerBox =
  | OuterBox of (Person.person) innerBox

val encodeOuterBox : outerBox -> Js_json.t

val decodeOuterBox : Js_json.t -> (outerBox, string) Belt.Result.t

type auditAction =
  | Create
  | Delete
  | Update

val encodeAuditAction : auditAction -> Js_json.t

val decodeAuditAction : Js_json.t -> (auditAction, string) Belt.Result.t

type 'a0 auditModel =
  { auditModel : 'a0
  ; originalId : string
  ; auditAction : auditAction
  ; editedBy : string
  ; editedOn : string
  }

val encodeAuditModel : ('a0 -> Js_json.t) -> 'a0 auditModel -> Js_json.t

val decodeAuditModel : (Js_json.t -> ('a0, string) Belt.Result.t) -> Js_json.t -> ('a0 auditModel, string) Belt.Result.t

type user =
  { userIdent : string
  ; userPassword : (string) option
  ; userTimezone : string
  }

val encodeUser : user -> Js_json.t

val decodeUser : Js_json.t -> (user, string) Belt.Result.t

type userAudit =
  | UserAudit of (user) auditModel

val encodeUserAudit : userAudit -> Js_json.t

val decodeUserAudit : Js_json.t -> (userAudit, string) Belt.Result.t
