{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Prelude hiding ((||), pure, id)
import Control.Arrow (Kleisli (..))
import Control.Concurrent.Async.Lifted
import Control.CSD.Network
import Control.CSD.CSD
import System.Environment

-------------------------------------------------------------------------------
-- Example 1: Distributed Compilation Service

priceOf :: String -> Int
priceOf "TaPL" = 80
priceOf "PFPL" = 100
priceOf _      = 999_999_999

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
data Buyer2
data Seller
data Seller2

loopN :: Int -> CSD f a a -> CSD f a a
loopN n f
  | n == 0 = Perm Id
  | n > 0 = f >>> loopN (n - 1) f
  | n < 0 = f >>> loopN (n + 1) f

priceOf' :: String -> Either String Int
priceOf' "HoTT" = Right 123
priceOf' "MLTT" = Right 456
priceOf' s      = Left s

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
  --   ["bookstore1"] -> do
  --     s1 <- async (return ())
  --     s2 <- async (return ())
  --     (s1', s2') <- runCSD runKleisli (bookstore1 @Buyer @Seller) (s1, s2)
  --     mapM_ wait [s1', s2']
  --   ["bookstore1", "buyer"] -> do
  --     s1 <- async (return ())
  --     let prog = project @Buyer runKleisli (bookstore1 @Buyer @Seller) (s1, absent)
  --     (s1, _) <- runHttpDebug config 40001 prog
  --     mapM_ wait [s1]
  --   ["bookstore1", "seller"] -> do
  --     s2 <- async (return ())
  --     let prog = project @Seller runKleisli (bookstore1 @Buyer @Seller) (absent, s2)
  --     (_, s2) <- runHttpDebug config 40002 prog
  --     mapM_ wait [s2]
  --   ["bookstore2"] -> do
  --     s1 <- async (return ())
  --     s2 <- async (return ())
  --     s3 <- async (return ())
  --     ((s1, s4), (s2, s3)) <- runCSD runKleisli bookstore2 (s1, (s2, s3))
  --     mapM_ wait [s1, s2, s3, s4]
  --   ["bookstore2", "buyer"] -> do
  --     s1 <- async (return ())
  --     let prog = project @Buyer runKleisli bookstore2 (s1, (absent, absent))
  --     ((s1, _), (_, _)) <- runHttpDebug config 40001 prog
  --     mapM_ wait [s1]
  --   ["bookstore2", "seller"] -> do
  --     s3 <- async (return ())
  --     let prog = project @Seller runKleisli bookstore2 (absent, (absent, s3))
  --     ((_, s4), (_, s3)) <- runHttp config 40002 prog
  --     mapM_ wait [s3, s4]
  --   ["bookstore2", "buyer2"] -> do
  --     s2 <- async (return ())
  --     let prog = project @Buyer2 runKleisli bookstore2 (absent, (s2, absent))
  --     ((_, _), (s2, _)) <- runHttp config 40003 prog
  --     mapM_ wait [s2]
  --   ["bookstore3"] -> do
  --     s1 <- async (return ())
  --     s2 <- async (return ())
  --     s3 <- async (return ())
  --     (s1', (s2', s3')) <- runCSD runKleisli bookstore3 (s1, (s2, s3))
  --     mapM_ wait [s1', s2', s3']
  --   ["bookstore3", "buyer"] -> do
  --     s1 <- async (return ())
  --     let prog = project @Buyer runKleisli bookstore3 (s1, (absent, absent))
  --     (s1', (_, _)) <- runHttp config 40001 prog
  --     mapM_ wait [s1']
  --   ["bookstore3", "seller"] -> do
  --     s2 <- async (return ())
  --     let prog = project @Seller runKleisli bookstore3 (absent, (s2, absent))
  --     (_, (s2', _)) <- runHttp config 40002 prog
  --     mapM_ wait [s2']
  --   ["bookstore3", "seller2"] -> do
  --     s3 <- async (return ())
  --     let prog = project @Seller2 runKleisli bookstore3 (absent, (absent, s3))
  --     (_, (_, s3')) <- runHttp config 40004 prog
  --     mapM_ wait [s3']
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
        ("Seller", ("localhost", 40002)),
        ("Buyer2", ("localhost", 40003)),
        ("Seller2", ("localhost", 40004))
      ]

    config2 :: HttpConfig
    config2 = mkHttpConfig
      [ ("LibRepo", ("localhost", 40001)),
        ("Server",  ("localhost", 40002)),
        ("ExeRepo", ("localhost", 40003))
      ]
