{-# LANGUAGE DataKinds #-}

-- based on HasChor (https://github.com/gshen42/HasChor/blob/async/src/Choreography/Network/Http.hs)
-- changed to final-tagless style and a few other changes
module Control.CSD.Network where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Monad (when, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HM
import Data.Proxy
import Network.HTTP.Client qualified as Http.Client
import Network.Wai.Handler.Warp (run)
import Servant.API hiding (Host)
import Servant.Client (BaseUrl(..), ClientM, client, mkClientEnv, runClientM, Scheme(Http))
import Servant.Server (Server, Handler, serve)

---------------------------------------------------------------------------------------------------
-- * Network Programs

type LocTm = String
type Id = Int

class (Monad m) => Network m where
  send :: (Show a) => LocTm -> Id -> Async a -> m (Async ())
  recv :: (Read a) => Id -> m (Async a)
  bcast :: (Show a) => Id -> a -> m [Async ()]

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

server :: Bool -> MsgBuf -> Server API
server debug buf = handler
  where
    handler :: Id -> String -> Handler NoContent
    handler id msg = do
      when debug (liftIO $ logMsg ("Received " ++ msg ++ " with sequence number " ++ show id))
      liftIO $ putMsg msg id buf
      return NoContent

-----------------------------------------------------------
-- Http configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

locs :: HttpConfig -> [LocTm]
locs = HM.keys . locToUrl

type Host = String
type Port = Int

-- | Create a HTTP backend configuration from an association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
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
  debug :: Bool,
  cfg :: HttpConfig,
  mgr :: Http.Client.Manager,
  buf :: MsgBuf
}

newtype Http a = Http { unHttp :: ReaderT HttpCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HttpCtx)

instance Network Http where
  send dst id a = do
    HttpCtx { debug, cfg, mgr } <- ask
    let env = mkClientEnv mgr (locToUrl cfg ! dst)
    async $ do
      a <- wait a
      when debug (liftIO $ logMsg ("Send " ++ show a ++ " to " ++ dst ++ " with id " ++ show id))
      res <- runClientM (sendServant id (show a)) env
      either print (void . return) res -- TODO: consider doing retry

  recv id = do
    HttpCtx {debug, buf} <- ask
    when debug (liftIO $ logMsg ("Wait for a message with id " ++ show id))
    async $ do
      read <$> getMsg id buf

  bcast id a = do
    HttpCtx { cfg } <- ask
    a' <- async (return a)
    mapM (\x -> send x id a') (locs cfg)

-- the top-most function
-- the second argument specifies the port to listen for incoming messages
runHttpTop :: HttpConfig -> Port -> Bool -> Http a -> IO a
runHttpTop cfg self debug m = do
  -- initialization
  mgr <- Http.Client.newManager Http.Client.defaultManagerSettings
  buf <- liftIO emptyMsgBuf
  let ctx = HttpCtx { debug, cfg, mgr, buf }

  -- start the server thread
  let serverPort = self
  let app = serve api (server debug buf)
  _ <- forkIO $ run serverPort app

  a <- runReaderT (unHttp m) ctx

  -- wait for pending sends to finish   TODO: collect these sends and wait on them
  -- also give the server thread some time to send back response (this probabaly can't be fixed)
  threadDelay 10_000_000
  return a

  -- TODO: kill the server thread?

logMsg :: String -> IO ()
logMsg msg = putStrLn ("* log: " ++ msg)

runHttp :: HttpConfig -> Port -> Http a -> IO a
runHttp cfg self= runHttpTop cfg self False

runHttpDebug :: HttpConfig -> Port -> Http a -> IO a
runHttpDebug cfg self = runHttpTop cfg self True
