let () =
  AesonSpec.goldenDirSpec
    (SharedTypes.decodeEntity AesonSpec.decodeIntWithResult AesonSpec.decodeIntWithResult)
    (SharedTypes.encodeEntity Aeson.Encode.int Aeson.Encode.int)
    "entity"
    "test/golden/Entity";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeKey
    SharedTypes.encodeKey
    "key"
    "test/golden/Key";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeTodoId
    SharedTypes.encodeTodoId
    "todoId"
    "test/golden/TodoId";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeUserId
    SharedTypes.encodeUserId
    "userId"
    "test/golden/UserId";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeUsername
    SharedTypes.encodeUsername
    "username"
    "test/golden/Username";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeTodo
    SharedTypes.encodeTodo
    "todo"
    "test/golden/Todo";

  AesonSpec.goldenDirSpec
    SharedTypes.decodeUser
    SharedTypes.encodeUser
    "user"
    "test/golden/User";
