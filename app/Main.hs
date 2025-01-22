{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Async
import Control.CSD.Site
import Control.CSD.CSD
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

bookstore1 :: (CSD f) => f (Site String, Site ()) (Site Int, Site ())
bookstore1 =
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

bookstore2 :: (CSD f) => f ((Site String, Site ()), (Site String, Site ())) ((Site Int, Site ()), (Site Int, Site ()))
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

-- bookstore3 :: CSD (Kleisli IO) (Site String, (Site (), Site ())) (Site Int, (Site (), Site ()))
-- bookstore3 =
--       (perf (\s -> return ((), s)) >>> fork) *** noop *** noop
--   >>> Perm (Trans AssocR (CongL AssocL))
--   >>> noop *** (joinL >>> perf (\(t, _) -> return t)) *** noop
--   >>> noop *** perf (\t -> return (priceOf' t)) *** noop
--   >>> noop *** Splt *** noop
--   >>> noop *** Ntfy
--   >>> noop *** Brch bookstore1 (noop *** noop)
--   >>> noop *** Idem
--   >>> noop *** perf (\p -> return (p, ())) *** noop
--   >>> noop *** fork *** noop
--   >>> Perm (Trans AssocL (Trans (CongR AssocL) AssocR))
--   >>> (joinR >>> perf (\(_, p) -> return p)) *** noop *** noop

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bookstore1"] -> do
      s1 <- async (putStrLn "Type in the title of the book:" >> getLine)
      s2 <- async (return ())
      (s1', s2') <- runCSD bookstore1 (s1, s2)
      s1'' <- async (wait s1' >>= print)
      mapM_ wait [s1'', s2']
    ["bookstore1", "buyer"] -> do
      s1 <- async (putStrLn "Type in the title of the book:" >> getLine)
      s2 <- async (return ())
      (Here s1, There _ s2) <- project bookstore1 (Here s1, There "2.2.2.2:8080" s2)
      s1 <- async (wait s1 >>= print)
      mapM_ wait [s1, s2]
    ["bookstore1", "seller"] -> do
      s1 <- async (return ())
      s2 <- async (return ())
      (There _ s1, Here s2) <- project bookstore1 (There "1.1.1.1:8080" s1, Here s2)
      mapM_ wait [s1, s2]
    ["bookstore2"] -> do
      s11 <- async (putStrLn "Buyer 1: Type in the title of the book:" >> getLine)
      s12 <- async (return ())
      s21 <- async (putStrLn "Buyer 2: Type in the title of the book:" >> getLine)
      s22 <- async (return ())
      ((s11, s12), (s21, s22)) <- runCSD bookstore2 ((s11, s12), (s21, s22))
      s11 <- async (wait s11 >>= print)
      s21 <- async (wait s21 >>= print)
      mapM_ wait [s11, s12, s21, s22]
    ["bookstore2", "buyer1"] -> do
      s11 <- async (putStrLn "Buyer 1: Type in the title of the book:" >> getLine)
      s12 <- async (return ())
      s21 <- async (return ())
      s22 <- async (return ())
      ((Here s11, There _ s12), (There _ s21, There _ s22)) <- project bookstore2 ((Here s11, There "3.3.3.3" s12), (There "2.2.2.2" s21, There "3.3.3.3" s22))
      s11 <- async (wait s11 >>= print)
      mapM_ wait [s11, s12, s21, s22]
    ["bookstore2", "buyer2"] -> do
      s11 <- async (return ())
      s12 <- async (return ())
      s21 <- async (putStrLn "Buyer 2: Type in the title of the book:" >> getLine)
      s22 <- async (return ())
      ((There _ s11, There _ s12), (Here s21, There _ s22)) <- project bookstore2 ((There "1.1.1.1" s11, There "3.3.3.3" s12), (Here s21, There "3.3.3.3" s22))
      s21 <- async (wait s21 >>= print)
      mapM_ wait [s11, s12, s21, s22]
    ["bookstore2", "seller"] -> do
      s11 <- async (return ())
      s12 <- async (return ())
      s21 <- async (return ())
      s22 <- async (return ())
      ((There _ s11, Here s12), (There _ s21, Here s22)) <- project bookstore2 ((There "1.1.1.1" s11, Here s12), (There "2.2.2.2" s21, Here s22))
      mapM_ wait [s11, s12, s21, s22]

    -- ["bookstore3"] -> do
    --   s1 <- async (putStrLn "Type in the title of the book:" >> getLine)
    --   s2 <- async (return ())
    --   s3 <- async (return ())
    --   let prog = interpAsynced runKleisli bookstore3
    --   (s1', (s2', s3')) <- prog (s1, (s2, s3))
    --   s1'' <- async (wait s1' >>= print)
    --   mapM_ wait [s1'', s2', s3']
    _ -> putStrLn "Unknown command-line arguments"
