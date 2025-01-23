{-# LANGUAGE DataKinds #-}

-- based on HasChor (https://github.com/gshen42/HasChor/blob/async/src/Choreography/Network/Http.hs)
-- changed to final-tagless style and a few other changes
module Control.Monad.Network where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Future
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HM
import Data.Proxy
import Network.HTTP.Client qualified as Http.Client
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client (BaseUrl(..), ClientM, client, mkClientEnv, runClientM, Scheme(Http))
import Servant.Server (Server, Handler, serve)

---------------------------------------------------------------------------------------------------
-- * Network Programs

type Loc = String
type Id = Int

class (Monad m) => Network m where
  send :: (Show a) => Loc -> Id -> a -> m ()
  recv :: (Read a) => Loc -> Id -> m a

---------------------------------------------------------------------------------------------------
-- * HTTP (Servant) Backend

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
      liftIO $ putStrLn "here"
      liftIO $ logMsg ("Received " ++ msg ++ " with id " ++ show id)
      liftIO $ putMsg msg id buf
      return NoContent

-----------------------------------------------------------
-- Http configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap Loc BaseUrl
  }

type Host = String
type Port = Int

-- | Create a HTTP backend configuration from an association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(Loc, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HM.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) =
      BaseUrl
        { baseUrlScheme = Servant.Client.Http,
          baseUrlHost = host,
          baseUrlPort = port,
          baseUrlPath = ""
        }

-----------------------------------------------------------
-- Put everything together, the `Http` monad

data HttpCtx = HttpCtx {
  cfg :: HttpConfig,
  mgr :: Http.Client.Manager,
  buf :: MsgBuf
}

newtype Http a = Http { unHttp :: ReaderT HttpCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HttpCtx, MonadFuture Async)

instance Network Http where
  send dst id a = do
    liftIO $ logMsg ("Send " ++ show a ++ " to " ++ show dst ++ " with id " ++ show id)
    HttpCtx { cfg, mgr } <- ask
    liftIO $ do
      let env = mkClientEnv mgr (locToUrl cfg ! dst)
      res <- runClientM (sendServant id (show a)) env
      either (logMsg . show) (void . return) res -- TODO: consider doing retry

  recv dst id = do
    liftIO $ logMsg ("Wait for a message from " ++ show dst ++ " with id " ++ show id)
    HttpCtx {buf} <- ask
    liftIO $ read <$> getMsg id buf

-- the top-most function
-- the second argument specifies the port to listen for incoming messages
runHttp :: HttpConfig -> Port -> Http a -> IO a
runHttp cfg self m = do
  -- initialization
  mgr <- Http.Client.newManager Http.Client.defaultManagerSettings
  buf <- liftIO emptyMsgBuf
  let ctx = HttpCtx { cfg, mgr, buf }

  -- start the server thread
  let serverPort = self
  let app = serve api (server buf)
  _ <- forkIO $ run serverPort app

  runReaderT (unHttp m) ctx

  -- TODO: kill the server thread?

logMsg :: String -> IO ()
logMsg msg = putStrLn ("* Http backend: " ++ msg)
