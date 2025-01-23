{-# LANGUAGE DataKinds #-}

-- based on HasChor (https://github.com/gshen42/HasChor/blob/async/src/Choreography/Network/Http.hs)
-- changed to final-tagless style and a few other changes
module Control.CSD.Network
  ( Host
  , Port
  , Url
  , Id
  , Network
  , send
  , recv
  , runNetwork
  ) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Proxy
import Network.HTTP.Client qualified as Http.Client
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client (BaseUrl(..), ClientM, client, mkClientEnv, runClientM, Scheme(Http))
import Servant.Server (Server, Handler, serve)

-----------------------------------------------------------
-- Servant API

type API =
  "send"
    :> Capture "id" Id
    :> ReqBody '[PlainText] String
    :> PostNoContent

api :: Proxy API
api = Proxy

-----------------------------------------------------------
-- Client action

sendServant :: Id -> String -> ClientM NoContent
sendServant = client api

-----------------------------------------------------------
-- Server action

type MsgBuf = MVar (HashMap Id (MVar String))

emptyMsgBuf :: IO MsgBuf
emptyMsgBuf = newMVar HM.empty

lookupMsgBuf :: Id -> MsgBuf -> IO (MVar String)
lookupMsgBuf id buf = do
  map <- takeMVar buf
  case HM.lookup id map of
    Just mvar -> do
      putMVar buf map
      return mvar
    Nothing -> do
      mvar <- newEmptyMVar
      putMVar buf (HM.insert id mvar map)
      return mvar

putMsg :: (Show a) => a -> Id -> MsgBuf -> IO ()
putMsg msg id buf = do
  mvar <- lookupMsgBuf id buf
  putMVar mvar (show msg)

-- bLocking semantics
getMsg :: (Read a) => Id -> MsgBuf -> IO a
getMsg id buf = do
  mvar <- lookupMsgBuf id buf
  read <$> takeMVar mvar

server :: MsgBuf -> Server API
server buf = handler
  where
    handler :: Id -> String -> Handler NoContent
    handler id msg = do
      liftIO $ logMsg ("Received " ++ msg ++ " with id " ++ show id)
      liftIO $ putMsg msg id buf
      return NoContent

-----------------------------------------------------------
-- Put everything together

type Port = Int
type Host = String
type Url = (Host, Port)

type Id = Int

data Ctx = Ctx {
  mgr :: Http.Client.Manager,
  buf :: MsgBuf
}

newtype Network a = Http { runHttp :: ReaderT Ctx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Ctx)

logMsg :: String -> IO ()
logMsg msg = putStrLn ("* Http backend: " ++ msg)

toBaseUrl :: Url -> BaseUrl
toBaseUrl (host, port) = BaseUrl {
  baseUrlScheme = Servant.Client.Http,
  baseUrlHost = host,
  baseUrlPort = port,
  baseUrlPath = ""
}

send :: (Show a) => Url -> Id -> a -> Network ()
send dst id a = do
  liftIO $ logMsg ("Send " ++ show a ++ " to " ++ show dst ++ " with id " ++ show id)
  Ctx { mgr } <- ask
  liftIO $ do
    let env = mkClientEnv mgr (toBaseUrl dst)
    res <- runClientM (sendServant id (show a)) env
    either (logMsg . show) (void . return) res -- TODO: consider doing retry

recv :: (Read a) => Url -> Id -> Network a
recv src id = do
  liftIO $ logMsg ("Wait for a message from " ++ show src ++ " with id " ++ show id)
  Ctx {buf} <- ask
  liftIO $ read <$> getMsg id buf

-- the top-most function
-- the second argument specifies the port to listen for incoming messages
runNetwork :: Network a -> Port -> IO a
runNetwork m self = do
  -- initialization
  mgr <- Http.Client.newManager Http.Client.defaultManagerSettings
  buf <- liftIO emptyMsgBuf
  let ctx = Ctx { mgr, buf }

  -- start the server thread and the main thread
  let serverPort = self
  let app = serve api (server buf)
  bracket
    (forkIO $ run serverPort app)
    killThread
    (\_ -> runReaderT (runHttp m) ctx)
