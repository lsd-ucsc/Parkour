{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Prelude hiding ((||))
import Control.Arrow
import Control.Concurrent.Async.Lifted
import Control.CSD.Network
import Control.CSD.CSD
import Data.Proxy
import Data.Typeable
import System.Environment

priceOf :: String -> Int
priceOf "TaPL" = 80
priceOf "PFPL" = 100
priceOf _      = 999_999_999

type Path = String

data Code
data Obj
data Exe
data LibRepo 
data Server
data ExeRepo

getSrc :: Path -> IO Code
getSrc = undefined

compile :: Code -> IO Obj
compile = undefined

link :: (Obj, Obj) -> IO Exe
link = undefined

distComp :: (Show Code, Read Code, Show Obj, Read Obj, Show Exe, Read Exe) => CSD (Kleisli IO)
  (Path @ LibRepo * () @ Server * Path @ ExeRepo)
  (Obj  @ LibRepo * () @ Server * () @ Server * Exe @ ExeRepo)
distComp = 
  -- repositories prepare and send source code
  perf getSrc || (perf (\x -> pure (x, x)) |> Fork) || perf getSrc                             |>
  (perf (\x -> pure ((), x)) |> Fork) || noop || noop || (perf (\x -> pure (x, ())) |> Fork)   |>
  noop || Comm || noop || noop || Comm || noop                                                 |>
  noop || (Join |> perf (\(x, _) -> pure x)) || (Join |> perf (\(_, x) -> pure x)) || noop     |>
  -- server compiles source code
  noop || perf compile || perf compile || noop                                                 |>
  noop || (perf (\x -> pure (x, x)) |> Fork) || noop || noop                                   |>
  noop || noop || Join || noop                                                                 |>
  noop || noop || perf link || noop                                                            |> 
  -- server sends back the results
  noop || (perf (\x -> pure (x, ())) |> Fork) || (perf (\x -> pure ((), x)) |> Fork) || noop   |>
  noop || Comm || noop || noop || Comm || noop                                                 |>
  (Join |> perf (\(_, x) -> pure x)) || noop || noop || (Join |> perf (\(x, _) -> pure x))      

-- seqExample :: (Typeable l) =>
--               (a -> b) -> (b -> c) -> CSD (Kleisli IO) (a @ l) (c @ l) 
-- seqExample f g = _

-- commExample :: (Typeable la, Typeable lb, Monad m, Show a, Read a) => 
--                CSD (Kleisli m) (a @ la,  b @ lb) (a @ la, (a, b) @ lb)
-- commExample
--   =   perf (\a -> return (a, a)) *** Noop
--   >>> Fork *** Noop
--   >>> AssocR 
--   >>> Noop *** (To *** Noop)
--   >>> Noop *** Join

-- parExample :: (Typeable la, Typeable lb, Typeable lc, Monad m, Show a, Read a, Show c, Read c) => 
--               CSD (Kleisli m) (a @ la, (c @ lc, b @ lb)) ((a @ la, (a ,b) @ lb), (c @ lc, (c, b) @ lb))
-- parExample 
--   =   Noop *** Noop *** (perf (\b -> return (b, b)) >>> Fork)
--   >>> (congL AssocL >>> congL Swap >>> AssocL)
--   >>> commExample *** commExample

-- data Buyer
-- data Buyer2
-- data Seller
-- data Seller2

-- -- Example 1: Basic Bookstore
-- --
-- -- A buyer sends the title of the book to the seller, then gets back its price.
-- --
-- --                > String        > ()
-- --                   |\            |
-- --                   | ¯¯¯¯¯¯¯¯¯¯| |
-- --                   |            \|
-- --                   |          priceOf
-- --                   |            /|
-- --                   | |¯¯¯¯¯¯¯¯¯¯ |
-- --                   |/            |
-- --                 < Int          < ()

-- bookstore1 :: forall buyer seller. (Typeable buyer, Typeable seller) =>
--               CSD (Kleisli IO) (() @ buyer, () @ seller) (() @ buyer, () @ seller)
-- bookstore1 =
--   perf (\_ -> getLine)        *** Noop >>> -- (String @ buyer, () @ seller)
--   perf (\t -> return ((), t)) *** Noop >>> -- (((), String) @ buyer, () @ seller)
--   Fork                        *** Noop >>> -- ((() @ buyer, String @ buyer), () @ seller)
--   AssocR                               >>> -- (() @ buyer, (String @ buyer, () @ seller))
--   Noop *** (To @_ @seller *** Noop)    >>> -- (() @ buyer, (String @ seller, () @ seller))
--   Noop *** Join                        >>> -- (() @ buyer, (String, ()) @ seller)
--   Noop *** perf (\(t, _) -> return (priceOf t, ())) >>> -- (() @ buyer, (Int, ()) @ seller)
--   Noop *** Fork                                     >>> -- (() @ buyer, (Int @ seller, () @ seller))
--   AssocL                                            >>> -- ((() @ buyer, Int @ seller), () @ seller)
--   (Noop *** To) *** Noop                            >>> -- ((() @ buyer, Int @ buyer), () @ seller)
--   Join *** Noop                                     >>> -- (((), Int) @ buyer), () @ seller)
--   perf (\(_, p) -> print p) *** Noop                    -- (() @ buyer, () @ seller)

-- bookstore1' :: forall buyer seller. (Typeable buyer, Typeable seller) => 
--                CSD (Kleisli IO) (String @ buyer, () @ seller) (Int @ buyer, () @ seller)
-- bookstore1' =
--   (perf (\t -> return ((), t)) >>> Fork) *** Noop                                         >>> 
--   AssocR                                                                                  >>> 
--   Noop *** ((To *** Noop) >>> Join >>> perf (\(t, _) -> return (priceOf t, ())) >>> Fork) >>> 
--   AssocL                                                                                  >>> 
--   ((Noop *** To) >>> Join >>> perf (\(_, p) -> return p)) *** Noop

-- -- Example 2: Parallel Bookstore
-- --
-- -- Two buyers interact with a seller in parallel.
-- --
-- -- It might seem that we have seller, but they should be considered as *logical* sellers.
-- -- When acutally running this choreography, we expect the *physical* seller would take the
-- -- role of both logical seller and run them in parallel.

-- loopN :: Int -> CSD f a a -> CSD f a a
-- loopN n f
--   | n == 0 = Noop
--   | n > 0 = f >>> loopN (n - 1) f
--   | n < 0 = f >>> loopN (n + 1) f

-- bookstore2 :: CSD (Kleisli IO) (() @ Buyer, (() @ Buyer2, () @ Seller)) ((() @ Buyer, () @ Seller), (() @ Buyer2, () @ Seller))
-- bookstore2 =
--       Noop *** Noop *** (perf (\_ -> return ((), ())) >>> Fork)
--   >>> (CongL AssocL >>> CongL Swap >>> AssocL)
--   >>> loopN 3 (bookstore1 *** bookstore1)

-- priceOf' :: String -> Either String Int
-- priceOf' "HoTT" = Right 123
-- priceOf' "MLTT" = Right 456
-- priceOf' s      = Left s

-- -- Example 3: Conditional Choreography (with granular knowledge of choice)
-- --
-- -- Two sellers:
-- --    Seller1 uses `priceOf'`, which is partial
-- --    Seller1 forwards the title to seller 2 if it can't lookup its price.
-- --
-- --                > String        > ()         > ()
-- --                   |\            |            |
-- --                   | ¯¯¯¯¯¯¯¯¯¯| |            |
-- --                   |            \|            |
-- --                   |          priceOf'        |
-- --                   |             |           /
-- --                   |            / \         /
-- --                   |           / _ \_______/
-- --                   |          /  |  \     |
-- --                   |         /   |   \    |
-- --                   |       bookstore1 |   |
-- --                   |         |   |    |   |
-- --                   |          \__|____|___/
-- --                   |              |     |
-- --                   |             /|     |
-- --                   | |¯¯¯¯¯¯¯¯¯¯¯ |     |
-- --                   |/             |     |
-- --                  < Int         < ()   < ()

-- bookstore3 :: CSD (Kleisli IO) (() @ Buyer, (() @ Seller, () @ Seller2)) (() @ Buyer, (() @ Seller, () @ Seller2))
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
    _ -> putStrLn "Unknown command-line arguments"
  where
    config :: HttpConfig
    config = mkHttpConfig
      [ ("Buyer", ("localhost", 40001)),
        ("Seller", ("localhost", 40002)),
        ("Buyer2", ("localhost", 40003)),
        ("Seller2", ("localhost", 40004))
      ]
