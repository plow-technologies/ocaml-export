{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVarIO, writeTVar, TVar)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setPort, defaultSettings)
import Shared.Types
import Servant

data MockDB =
  MockDB
    { userTodos :: [(UserId, [EntityTodo])]
    , users     :: [EntityUser]
    -- , authuser :: Text
    -- , authkey :: Text
    } 

defaultMockDB :: MockDB
defaultMockDB =
  MockDB
    { userTodos =
      [ ( (UserId $ Key 0)
        , [ (Entity (TodoId $ Key 0) $ Todo "write the report" False)
          , (Entity (TodoId $ Key 1) $ Todo "take the kids to school" False)
          , (Entity (TodoId $ Key 2) $ Todo "review the PR" False)
          , (Entity (TodoId $ Key 3) $ Todo "have lunch with friends" False)
          ]
        )
      ]
    , users =
      [ Entity (UserId $ Key 0) $ User (Username "Jordan") "alpha"
      , Entity (UserId $ Key 1) $ User (Username "Jeb") "beta"
      , Entity (UserId $ Key 1) $ User (Username "Jamil") "gamma"
      ]
    }

getNewUserId :: MockDB -> UserId
getNewUserId = UserId . Key . fromIntegral . length . users

getNewTodoId :: MockDB -> TodoId
getNewTodoId mockDB = TodoId . Key . fromIntegral $ foldl (\count (_,todos) -> count + length todos) 0 (userTodos mockDB)

getUserTodos :: UserId -> MockDB -> [EntityTodo]
getUserTodos userId mockDB =
  let r = filter (\x -> userId == fst x) (userTodos mockDB)
  in if   length r > 0
     then snd $ head r
     else []

addUserTodo :: UserId -> EntityTodo -> MockDB -> MockDB
addUserTodo userId todo mockDB =
  let userElem _ [] = False
      userElem u1 ((u2,_):xs) = if u1 == u2 then True else userElem u1 xs

      userTodos' = userTodos mockDB
  in  if   userElem userId userTodos'
      then mockDB { userTodos = (\x -> if userId == (fst x) then ((\todos -> todos ++ [todo]) <$> x) else x) <$> userTodos' }
      else mockDB { userTodos = userTodos' ++ [(userId, [todo])] }

addUser :: EntityUser -> MockDB -> MockDB
addUser user mockDB = mockDB { users = users mockDB ++ [user] }


app :: TVar MockDB -> Application
app tMockDB = serve serverAPI $ server tMockDB

serverAPI :: Proxy API
serverAPI = Proxy

server :: TVar MockDB -> Server API
server tMockDB = (postTodoH :<|> getTodosH :<|> postUserH :<|> getUsersH :<|> (serveDirectoryWebApp "static"))
  where
    postTodoH userId todo = do
      mockDB <- liftIO $ readTVarIO tMockDB
      let todoEntity = Entity (getNewTodoId mockDB) todo
      liftIO $ atomically $ modifyTVar' tMockDB (addUserTodo userId todoEntity)
      pure todoEntity

    getTodosH userId = do
      mockDB <- liftIO $ readTVarIO tMockDB
      pure $ getUserTodos userId mockDB

    postUserH user = do
      mockDB <- liftIO $ readTVarIO tMockDB
      let userEntity = Entity (getNewUserId mockDB) user
      liftIO $ atomically $ modifyTVar' tMockDB (addUser userEntity)
      pure userEntity

    getUsersH = users <$> (liftIO $ readTVarIO tMockDB)


runServer :: IO ()
runServer = do
  tMockDB <- newTVarIO defaultMockDB
  let settings = setPort 8001 $ defaultSettings
  runSettings settings (app tMockDB)
