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
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application


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


-- REST API for todo items
restTodo :: Handler App (AuthManager App) ()
restTodo = do
  modifyResponse $ setContentType "application/json"
  method GET getTodo <|> method PUT saveTodo
  where
    getTodo =
      writeLBS "{ \"id\":\"3\", \"descr\":\"foo\" }"

    saveTodo = return ()


restTodoList :: Handler App (AuthManager App) ()
restTodoList = method GET $ do
  modifyResponse $ setContentType "application/json"
  writeLBS "[{ \"id\":\"3\", \"descr\":\"foo\" }, { \"id\":\"4\", \"descr\":\"paskaks\" }]"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",     with auth handleLoginSubmit)
         , ("/logout",    with auth handleLogout)
         , ("/new_user",  with auth handleNewUser)
         , ("/todo/:id",  with auth restTodo)
         , ("/todo/list", with auth restTodoList)
         , ("",           serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices auth
    return $ App h s d a

