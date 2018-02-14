let () =
  AesonSpec.goldenDirSpec
    Person.decodePerson
    Person.encodePerson
    "person"
    "golden/product/Person";
