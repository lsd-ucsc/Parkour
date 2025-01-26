{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Async.Lifted
import Control.CSD.Network
import Control.CSD.Site
import Control.CSD.CSD
import Control.CSD.ChoiceCSD
import System.Environment

priceOf :: String -> Int
priceOf "TaPL" = 80
priceOf "PFPL" = 100
priceOf _      = 999_999_999

-- Example 1: Basic Bookstore
--
-- A buyer sends the title of the book to the seller, then gets back its price.
--
--                > String        > ()
--                   |\            |
--                   | ¯¯¯¯¯¯¯¯¯¯| |
--                   |            \|
--                   |          priceOf
--                   |            /|
--                   | |¯¯¯¯¯¯¯¯¯¯ |
--                   |/            |
--                 < Int          < ()

bookstore1 :: (CSD f) => f (Site (), Site ()) (Site (), Site ())
bookstore1 =
      (perf (\_ -> putStrLn ">>> Type in the title of the book:" >> ((),) <$> getLine) >>> forkR) *** noop
  >>> assocR
  >>> noop *** (joinL >>> perf (\(_, t) -> return t))
  >>> noop *** perf (\t -> return (priceOf t, ()))
  >>> noop *** fork
  >>> assocL
  >>> (joinR >>> perf (\(_, p) -> print p)) *** noop

bookstore1' :: (CSD f) => f (Site String, Site ()) (Site Int, Site ())
bookstore1' =
      (perf (\s -> return ((), s)) >>> fork) *** noop
  >>> assocR
  >>> noop *** (joinL >>> perf (\(_, t) -> return t))
  >>> noop *** perf (\t -> return (priceOf t, ()))
  >>> noop *** fork
  >>> assocL
  >>> (joinR >>> perf (\(_, p) -> return p)) *** noop

-- Example 2: Parallel Bookstore
--
-- Two buyers interact with a seller in parallel.
--
-- It might seem that we have seller, but they should be considered as *logical* sellers.
-- When acutally running this choreography, we expect the *physical* seller would take the
-- role of both logical seller and run them in parallel.

loop :: (CSD f) => f a a -> f a a
loop f = f >>> loop f

bookstore2 :: (CSD f) => f (Site (), (Site (), Site ())) ((Site (), Site ()), (Site (), Site ()))
bookstore2 =
      noop *** noop *** (perf (\_ -> return ((), ())) >>> fork)
  >>> perm1
  >>> loop (bookstore1 *** bookstore1)
  where
    perm1 :: (CSD f) => f (Site (), (Site (), (Site (), Site ()))) ((Site (), Site ()), (Site (), Site ()))
    perm1 = trans (congL assocL) (trans (congL swap) assocL)

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

bookstore3 :: Int -> (ChoiceCSD f) => f (Site (), (Site (), Site ())) (Site (), (Site (), Site ()))
bookstore3 0 = noop *** noop *** noop
bookstore3 n =
      (perf (\_ -> putStrLn ">>> Type in the title of the book:" >> ((),) <$> getLine) >>> forkR) *** noop *** noop
  >>> trans assocR (congL assocL)
  >>> noop *** (joinL >>> perf (\(_, t) -> return t)) *** noop
  >>> noop *** perf (\t -> return (priceOf' t)) *** noop
  >>> noop *** split
  >>> noop *** branch bookstore1' (noop *** noop)
  >>> noop *** idem
  >>> noop *** (perf (\p -> return ((), p)) >>> forkL) *** noop
  >>> trans assocL (trans (congR assocL) assocR)
  >>> (joinR >>> perf (\(_, p) -> print p)) *** noop *** noop
  >>> bookstore3 (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bookstore1"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      (s1', s2') <- runCSD bookstore1 (s1, s2)
      mapM_ wait [s1', s2']
    ["bookstore1", "buyer"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      let prog = project bookstore1 (Self s1, Peer s2 "seller")
      (Self s1, Peer s2 _) <- runHttp config 40001 prog
      s1 <- async (wait s1 >>= print)
      mapM_ wait [s1, s2]
    ["bookstore1", "seller"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      let prog = project bookstore1 (Peer s1 "buyer", Self s2)
      (Peer s1 _, Self s2) <- runHttp config 40002 prog
      mapM_ wait [s1, s2]
    ["bookstore2"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      ((s1, s4), (s2, s3)) <- runCSD bookstore2 (s1, (s2, s3))
      mapM_ wait [s1, s2, s3, s4]
    ["bookstore2", "buyer"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project bookstore2 (Self s1, (Peer s2 "buyer2", Peer s3 "seller"))
      ((Self s1, Peer s4 _), (Peer s2 _, Peer s3 _)) <- runHttp config 40001 prog
      mapM_ wait [s1, s2, s3, s4]
    ["bookstore2", "seller"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project bookstore2 (Peer s1 "buyer", (Peer s2 "buyer2", Self s3))
      ((Peer s1 _, Self s4), (Peer s2 _,  Self s3)) <- runHttp config 40002 prog
      mapM_ wait [s1, s2, s3, s4]
    ["bookstore2", "buyer2"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project bookstore2 (Peer s1 "buyer", (Self s2, Peer s3 "seller"))
      ((Peer s1 _, Peer s4 _), (Self s2,  Peer s3 _)) <- runHttp config 40003 prog
      mapM_ wait [s1, s2, s3, s4]
    ["bookstore3"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      (s1', (s2', s3')) <- runCSD (bookstore3 3) (s1, (s2, s3))
      mapM_ wait [s1', s2', s3']
    ["bookstore3", "buyer"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project (bookstore3 3) (Self s1, (Peer s2 "seller", Peer s3 "seller2"))
      (Self s1', (Peer s2' _, Peer s3' _)) <- runHttp config 40001 prog
      mapM_ wait [s1', s2', s3']
    ["bookstore3", "seller"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project (bookstore3 3) (Peer s1 "buyer", (Self s2, Peer s3 "seller2"))
      (Peer s1' _, (Self s2', Peer s3' _)) <- runHttp config 40002 prog
      mapM_ wait [s1', s2', s3']
    ["bookstore3", "seller2"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = project (bookstore3 3) (Peer s1 "buyer", (Peer s2 "seller", Self s3))
      (Peer s1' _, (Peer s2' _, Self s3')) <- runHttp config 40004 prog
      mapM_ wait [s1', s2', s3']
    _ -> putStrLn "Unknown command-line arguments"
  where
    config :: HttpConfig
    config = mkHttpConfig
      [ ("buyer", ("localhost", 40001)),
        ("seller", ("localhost", 40002)),
        ("buyer2", ("localhost", 40003)),
        ("seller2", ("localhost", 40004))
      ]
