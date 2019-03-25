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

type innerBox =
  | OuterBox of innerBox

let (encodeInnerBox) x =
  match x with
  | OuterBox y0 ->
     (encodeInnerBox) y0

let (decodeInnerBox) json =
  match Aeson.Decode.decodeInnerBox json with
  | v -> Belt.Result.Ok (OuterBox v)
  | exception Aeson.Decode.DecodeError msg -> Belt.Result.Error ("decodeOuterBox: " ^ msg)
