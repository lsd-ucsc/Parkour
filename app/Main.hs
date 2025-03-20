{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Prelude hiding ((||), pure, id)
import Control.Arrow (Kleisli (..))
import Control.Concurrent.Async.Lifted
import Control.CSD.Network
import Control.CSD.CSD
import Data.Time
import System.Environment

-------------------------------------------------------------------------------
-- Example 1: Distributed Compilation Service

type Path = String
type Code = String
type Obj = String
type Exe = String

data LibRepo
data Server
data ExeRepo

getSrc :: Path -> IO Code
getSrc _ = getLine

compile :: Code -> IO Obj
compile c = do
  x <- getLine
  return (c ++ x)

link :: (Obj, Obj) -> IO Exe
link (o1, o2) = return (o1 ++ o2)

distComp :: (Show Code, Read Code, Show Obj, Read Obj, Show Exe, Read Exe) => CSD (Kleisli IO)
  (Path @ LibRepo * () @ Server * Path @ ExeRepo)
  (Obj  @ LibRepo * () @ Server * () @ Server * Exe @ ExeRepo)
distComp =
  -- repositories prepare and send source code
  perfM getSrc *** (pure (\x -> (x,x)) >>> Fork) *** perfM getSrc                     >>>~
  (pure (\x -> ((),x)) >>> Fork) *** noop *** noop *** (pure (\x -> (x,())) >>> Fork) >>>~
  noop *** Comm *** noop *** noop *** Comm *** noop                                   >>>~
  noop *** (Join >>> pure (\(x, _) -> x)) *** (Join >>> pure (\(_,x) -> x)) *** noop  >>>~
  -- server compiles source code
  noop *** perfM compile *** perfM compile *** noop                                   >>>~
  noop *** (pure (\x -> (x,x)) >>> Fork) *** noop *** noop                            >>>~
  noop *** noop *** Join *** noop                                                     >>>~
  noop *** noop *** perfM link *** noop                                               >>>~
  -- server sends back the results
  noop *** (pure (\x -> (x,())) >>> Fork) *** (pure (\x -> ((),x)) >>> Fork) *** noop >>>~
  noop *** Comm *** noop *** noop *** Comm *** noop                                   >>>~
  (Join >>> pure (\(_,x) -> x)) *** noop *** noop *** (Join >>> pure (\(x,_) -> x))

-------------------------------------------------------------------------------
-- Example 2: Bookstore

data Buyer
data Seller

getTitle :: () -> IO ((), String)
getTitle _ = do
  putStrLn "Type in the book you want to buy:"
  s <- getLine
  return ((), s)

getPrice :: (String, ()) -> IO (Int, ())
getPrice (s, _) = do
  putStrLn ("The buyer wants to buy " ++ s)
  putStrLn "What's the price of the book?"
  p <- read <$> getLine
  return (p, ())

decide :: ((), Int) -> IO (Either () ())
decide (_, p) = do
  putStrLn "What's your budget?"
  b <- read <$> getLine
  if b >= p
  then return (Right ())
  else return (Left ())

getDate :: () -> IO (Day, ())
getDate _ = do
  putStrLn "What's the delivery date of the book?"
  d <- read <$> getLine
  return (d, ())

bookstore :: CSD (Kleisli IO) (() @ Buyer * () @ Seller) (Either () Day @ Buyer * () @ Seller)
bookstore =
  perfM getTitle *** noop >>>~
  fork *** noop           >>>~
  noop *** comm *** noop  >>>~
  noop *** join           >>>~
  noop *** perfM getPrice >>>~
  noop *** fork           >>>~
  noop *** comm *** noop  >>>~
  join *** noop           >>>~
  perfM decide *** noop   >>>~
  split *** noop          >>>~
  distribIn               >>>~
  case1 ||| case2         >>>~
  distribOut              >>>~
  merge *** noop
  where
    case1 = pure (\_ -> ()) *** noop
    case2 =
      noop *** perfM getDate >>>~
      noop *** fork          >>>~
      noop *** comm *** noop >>>~
      join *** noop          >>>~
      pure (\(_,d) -> d) *** noop


-- Example 3: Conditional Choreography (with granular knowledge of choice)
--
-- Two sellers:
--    Seller1 uses `priceOf'`, which is partial
--    Seller1 forwards the title to seller 2 if it can't lookup its price.
--
--                > String        > ()         > ()
--                   |\            |            |
--                   | ¯¯¯¯¯¯¯¯¯¯| |            |
--                   |            \|            |
--                   |          priceOf'        |
--                   |             |           /
--                   |            / \         /
--                   |           / _ \_______/
--                   |          /  |  \     |
--                   |         /   |   \    |
--                   |       bookstore1 |   |
--                   |         |   |    |   |
--                   |          \__|____|___/
--                   |              |     |
--                   |             /|     |
--                   | |¯¯¯¯¯¯¯¯¯¯¯ |     |
--                   |/             |     |
--                  < Int         < ()   < ()

-- bookstore3 :: CSD (Kleisli IO) (() @ Buyer * () @ Seller * () @ Seller2) (() @ Buyer * () @ Seller * () @ Seller2)
-- bookstore3 =
--       (perf (\_ -> putStrLn ">>> Type in the title of the book:" >> ((),) <$> getLine) >>> Fork) *** Noop *** Noop
--   >>> (AssocR >>> CongL AssocL)
--   >>> Noop *** ((To *** Noop) >>> Join >>> perf (\(t, _) -> return t)) *** Noop
--   >>> Noop *** perf (\t -> return (priceOf' t)) *** Noop
--   >>> Noop *** Split *** Noop
--   >>> Noop *** Notify @Seller
--   >>> Noop *** Branch bookstore1' Noop
--   >>> Noop *** Idem
--   >>> Noop *** (perf (\p -> return (p, ())) >>> Fork) *** Noop
--   >>> (AssocL >>> CongR AssocL >>> AssocR)
--   >>> ((Noop *** To) >>> Join >>> perf (\(_, p) -> print p)) *** Noop *** Noop
--   >>> loopN 3 bookstore3

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bookstore"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      (s1, s2) <- runCSD runKleisli bookstore (s1, s2)
      s1 <- async (wait s1 >>= print)
      mapM_ wait [s1, s2]
    ["bookstore", "buyer"] -> do
        s1 <- async (return ())
        let prog = project @Buyer runKleisli bookstore (s1, absent)
        (s1, _) <- runHttpDebug config 40001 prog
        s1 <- async (wait s1 >>= print)
        mapM_ wait [s1]
    ["bookstore", "seller"] -> do
        s2 <- async (return ())
        let prog = project @Seller runKleisli bookstore (absent, s2)
        (_, s2) <- runHttpDebug config 40002 prog
        mapM_ wait [s2]
    ["distComp"] -> do
      s1 <- async (return "path1")
      s2 <- async (return ())
      s3 <- async (return "path2")
      (s1, (_, (_, s3)))<- runCSD runKleisli distComp (s1, (s2, s3))
      s1 <- async (wait s1 >>= print)
      s3 <- async (wait s3 >>= print)
      mapM_ wait [s1, s3]
    ["distComp", "LibRepo"] -> do
      s1 <- async (return "path1")
      let prog = project @LibRepo runKleisli distComp (s1, (absent, absent))
      (s1, (_, (_, _))) <- runHttpDebug config2 40001 prog
      s1 <- async (wait s1 >>= print)
      mapM_ wait [s1]
    ["distComp", "Server"] -> do
      s2 <- async (return ())
      let prog = project @Server runKleisli distComp (absent, (s2, absent))
      (_, (s2, (s2', _))) <- runHttpDebug config2 40002 prog
      mapM_ wait [s2, s2']
    ["distComp", "ExeRepo"] -> do
        s3 <- async (return "path2")
        let prog = project @ExeRepo runKleisli distComp (absent, (absent, s3))
        (_, (_, (_, s3))) <- runHttpDebug config2 40003 prog
        s3 <- async (wait s3 >>= print)
        mapM_ wait [s3]
    _ -> putStrLn "Unknown command-line arguments"
  where
    config :: HttpConfig
    config = mkHttpConfig
      [ ("Buyer", ("localhost", 40001)),
        ("Seller", ("localhost", 40002))
      ]

    config2 :: HttpConfig
    config2 = mkHttpConfig
      [ ("LibRepo", ("localhost", 40001)),
        ("Server",  ("localhost", 40002)),
        ("ExeRepo", ("localhost", 40003))
      ]
