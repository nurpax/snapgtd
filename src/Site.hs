{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Maybe
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Text.JSON
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application

import           Debug.Trace

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (bindSplices errs) $ render "login"
  where
    errs = [("loginError", textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


data Todo =
  Todo
  {
    todoId :: Int
  , todoDescr :: String
  } deriving (Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field

instance JSON Todo where
  readJSON object = do
    obj <- readJSON object
    Todo <$> valFromObj "id" obj
         <*> valFromObj "descr" obj

  showJSON obj = makeObj
                 [ ("id", showJSON $ todoId obj)
                 , ("descr",  showJSON $ todoDescr obj)]

-- REST API for todo items
restTodo :: Handler App App ()
restTodo = do
  modifyResponse $ setContentType "application/json"
  todoId <- getParam "id"
  maybe (return ()) go (toInt <$> todoId)
  where
    go :: Int -> Handler App App ()
    go todoId = do
      method GET (getTodo todoId) <|> method PUT (updateTodo todoId)

    toInt :: BS.ByteString -> Int
    toInt = read . T.unpack . T.decodeUtf8

    getTodo :: Int -> Handler App App ()
    getTodo todoId = do
      rows <- query "SELECT id,descr FROM todo WHERE id = ?" (Only todoId)
      writeText . T.pack . encode $ (head rows :: Todo)

    updateTodo :: Int -> Handler App App ()
    updateTodo todoId = do
      rqb <- getRequestBody
      -- TODO handle Either Left here
      let json = either (error . show) id (resultToEither . decode . bsToString $ rqb) :: Todo
      void $ execute "UPDATE todo SET descr = ? WHERE id = ?" (todoDescr json, todoId)

    bsToString :: LBS.ByteString -> String
    bsToString = T.unpack . T.decodeUtf8 . BS.concat . LBS.toChunks



restTodoList :: Handler App App ()
restTodoList = do
  modifyResponse $ setContentType "application/json"
  rows <- query_ "SELECT id,descr FROM todo"
  method GET (getTodos rows)
  where
    getTodos :: [Todo] -> Handler App App ()
    getTodos todos =
      writeText . T.pack . encode $ todos


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",     with auth handleLoginSubmit)
         , ("/logout",    with auth handleLogout)
         , ("/new_user",  with auth handleNewUser)
         , ("/todo/:id",  restTodo)
         , ("/todo/list", restTodoList)
         , ("",           serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices auth
    return $ App h s d a
