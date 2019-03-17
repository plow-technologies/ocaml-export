open SharedTypes;

let usernameToString = (username: username) =>
  switch (username) {
  | Username(s) => s
  };

module Fetch =
  ServerFetch.MakeServerFetch({
    let baseUrl = ServerFetchConfig.config.baseUrl;
  });

type page =
  | UsersPage
  | TodosPage;

type state = {
  users: array(entity(userId, user)),
  page,
  todos: array(entity(todoId, todo)),
};

let initialState = () => {users: [||], page: UsersPage, todos: [||]};

/* Action declaration */
type action =
  | UpdateUsers(array(entity(userId, user)))
  | UpdatePage(page)
  | FetchTodos(userId)
  | GotTodos(array(entity(todoId, todo)));

let reducer = (action, state) =>
  switch (action) {
  | UpdateUsers(users) => ReasonReact.Update({...state, users})
  | UpdatePage(page) => ReasonReact.Update({...state, page})
  | FetchTodos(userId) =>
    ReasonReact.UpdateWithSideEffects(
      state,
      (
        ({send}) =>
          Fetch.getUserTodos(userId)
          |> Js.Promise.then_(todos => {
               send(GotTodos(todos));
               Js.Promise.resolve();
             })
          |> ignore
      ),
    )
  | GotTodos(todos) => ReasonReact.Update({...state, todos})
  };

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
        if (state.page == UsersPage) {
          Array.map(
            (user: entity(userId, user)) =>
              <div onClick=(_ => send(FetchTodos(user.entityKey)))>
                (
                  ReasonReact.string(
                    usernameToString(user.entityValue.username),
                  )
                )
              </div>,
            state.users,
          )
          |> ReasonReact.array;
        } else {
          ReasonReact.null;
        }
      )
    </div>,
};