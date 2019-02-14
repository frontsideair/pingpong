module Lib where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors

ping :: Coroutine (Request String String) IO ()
ping = forever $ do
  a <- request "ping"
  liftIO $ putStrLn a

pong :: Coroutine (Request String String) IO ()
pong = forever $ do
  a <- request "pong"
  liftIO $ putStrLn a

pingpong :: Coroutine (Yield (String, String)) IO ((), ())
pingpong =
  weave sequentialBinder (weaveRequests "default1" "default2") ping pong

printProduce :: Show x => Coroutine (Yield x) IO r -> IO r
printProduce = pogoStick (\(Yield x cont) -> liftIO (print x) >> cont)

