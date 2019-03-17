open SharedTypes;

let usernameToString = (username: username) =>
  switch (username) {
  | Username(s) => s
  };

module Fetch =
  ServerFetch.MakeServerFetch({
    let baseUrl = ServerFetchConfig.config.baseUrl;
  });

type state = {users: array(entity(userId, user))};

let initialState = () => {users: [||]};

/* Action declaration */
type action =
  | UpdateUsers(array(entity(userId, user)))
  | ShowUserTodos(userId);

let reducer = (action, state) =>
  switch (action) {
  | UpdateUsers(users) => ReasonReact.Update({...state, users})
  | ShowUserTodos(userId) => ReasonReact.NoUpdate
  };

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("UserSelector");

let make = _children => {
  ...component,
  initialState,
  didMount: self =>
    Fetch.getUsers()
    |> Js.Promise.then_(rUsers => {
         self.send(UpdateUsers(rUsers));
         Js.Promise.resolve();
       })
    |> ignore,
  reducer,
  render: ({send, state}) =>
    <div>
      (
        Array.map(
          (user: entity(userId, user)) =>
            <div onClick=(_ => send(ShowUserTodos(user.entityKey)))>
              (
                ReasonReact.string(
                  usernameToString(user.entityValue.username),
                )
              )
            </div>,
          state.users,
        )
        |> ReasonReact.array
      )
    </div>,
};