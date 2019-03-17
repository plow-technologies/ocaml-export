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
  tMockDB <- newTVarIO $ MockDB [] []
  -- let settings = setHost (fromString . mscHost $ config) $ setPort (mscPort config) $ defaultSettings
  let settings = setPort 8001 $ defaultSettings
  runSettings settings (app tMockDB)
