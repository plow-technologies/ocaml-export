type sumWithRecordA1 =
  { a1 : int
  }

type sumWithRecordB2 =
  { b2 : string
  ; b3 : int
  }

type sumWithRecord =
  | A1 of sumWithRecordA1
  | B2 of sumWithRecordB2

val encodeSumWithRecordA1 : sumWithRecordA1 -> Js_json.t

val encodeSumWithRecordB2 : sumWithRecordB2 -> Js_json.t

val encodeSumWithRecord : sumWithRecord -> Js_json.t

val decodeSumWithRecordA1 : Js_json.t -> (sumWithRecordA1, string) Belt.Result.t

val decodeSumWithRecordB2 : Js_json.t -> (sumWithRecordB2, string) Belt.Result.t

val decodeSumWithRecord : Js_json.t -> (sumWithRecord, string) Belt.Result.t
