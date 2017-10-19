type subTypeParameter =
  { as : (a0) list
  }

let encodeSubTypeParameter (x : subTypeParameter) :Js_json.t =
  Json.Encode.object_
    [ ( "as", (Json.Encode.list encodea0) x.as )
    ]
