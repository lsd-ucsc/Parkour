{-# LANGUAGE GADTs #-}

module Main where

import Control.Arrow hiding ((>>>), (***))
import Control.Concurrent.Async
import Control.CSD
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

bookstore1 :: (Monad m) => CSD (Kleisli m) (Local String, Local ()) (Local Int, Local ())
bookstore1 =
      (perf (\s -> return ((), s)) >>> Fork) *** noop
  >>> Perm AssocR
  >>> noop *** (Join >>> perf (\(t, _) -> return t))
  >>> noop *** perf (\t -> return (priceOf t, ()))
  >>> noop *** Fork
  >>> Perm AssocL
  >>> (Join >>> perf (\(_, p) -> return p)) *** noop

-- Example 2: Parallel Bookstore
--
-- Two buyers interact with a seller in parallel.
--
-- It might seem that we have seller, but they should be considered as *logical* sellers.
-- When acutally running this choreography, we expect the *physical* seller would take the
-- role of both logical seller and run them in parallel.

bookstore2 :: CSD (Kleisli IO) ((Local String, Local ()), (Local String, Local ())) ((Local Int, Local ()), (Local Int, Local ()))
bookstore2 = bookstore1 *** bookstore1

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

bookstore3 :: CSD (Kleisli IO) (Local String, (Local (), Local ())) (Local Int, (Local (), Local ()))
bookstore3 =
      (perf (\s -> return ((), s)) >>> Fork) *** noop *** noop
  >>> Perm (Trans AssocR (CongL AssocL))
  >>> noop *** (Join >>> perf (\(t, _) -> return t)) *** noop
  >>> noop *** perf (\t -> return (priceOf' t)) *** noop
  >>> noop *** Split *** noop
  >>> noop *** Notify
  >>> noop *** Branch bookstore1 (noop *** noop)
  >>> noop *** Perm Idem
  >>> noop *** perf (\p -> return (p, ())) *** noop
  >>> noop *** Fork *** noop
  >>> Perm (Trans AssocL (Trans (CongR AssocL) AssocR))
  >>> (Join >>> perf (\(_, p) -> return p)) *** noop *** noop

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bookstore1"] -> do
      s1 <- async (putStrLn "Type in the title of the book:" >> getLine)
      s2 <- async (return ())
      let prog = interpAsynced runKleisli bookstore1
      (s1', s2') <- prog (s1, s2)
      s1'' <- async (wait s1' >>= print)
      mapM_ wait [s1'', s2']
    ["bookstore2"] -> do
      s11 <- async (putStrLn "Buyer 1: Type in the title of the book:" >> getLine)
      s12 <- async (return ())
      s21 <- async (putStrLn "Buyer 2: Type in the title of the book:" >> getLine)
      s22 <- async (return ())
      let prog = interpAsynced runKleisli bookstore2
      ((s11', s12'), (s21', s22')) <- prog ((s11, s12), (s21, s22))
      s11'' <- async (wait s11' >>= print)
      s21'' <- async (wait s21' >>= print)
      mapM_ wait [s11'', s12', s21'', s22']
    ["bookstore3"] -> do
      s1 <- async (putStrLn "Type in the title of the book:" >> getLine)
      s2 <- async (return ())
      s3 <- async (return ())
      let prog = interpAsynced runKleisli bookstore3
      (s1', (s2', s3')) <- prog (s1, (s2, s3))
      s1'' <- async (wait s1' >>= print)
      mapM_ wait [s1'', s2', s3']
    _ -> putStrLn "Unknown command-line arguments"
