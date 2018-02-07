type ('a0, 'a1) result =
  | Success of 'a0
  | Error of 'a1

val encodeResult : ('a0 -> Js_json.t) -> ('a1 -> Js_json.t) -> ('a0, 'a1) result -> Js_json.t

val decodeResult : (Js_json.t -> ('a0, string) Js_result.t) -> (Js_json.t -> ('a1, string) Js_result.t) -> Js_json.t -> (('a0, 'a1) result, string) Js_result.t

type ('a0, 'a1, 'a2) sumWithRecordCR4 =
  { cr4b : 'a1
  ; cr4av : ('a0 * 'a2)
  }
                                                
type ('a0, 'a1, 'a2) complexResult =
  | CR0 of 'a0
  | CR1 of 'a0 * 'a1
  | CR2 of 'a1 * ('a2 * 'a0)
  | CR3 of string * 'a1 * int * 'a0
  | CR4 of ('a0, 'a1, 'a2) sumWithRecordCR4
  | CR5
