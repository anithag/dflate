{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Data.Binary
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getArgs)
import Foreign.StablePtr

import Simulator



main :: IO ()
main = do
  (tp:rest) <- getArgs
  case tp of
    "worker" -> case rest of
      [port] -> do
        putStrLn $ "starting worker on " ++ port
        t <- createTransport "10.0.3.6" port (\port'-> ("10.0.3.6", port') ) defaultTCPParameters
        case t of 
          Right transport -> do
             putStrLn $ "started worker on " ++ port
             node <- newLocalNode transport myRemoteTable
             windowToEternity <- newEmptyMVar
             _ <- newStablePtr =<< myThreadId
             putStrLn $ "waiting for work to come.."
             takeMVar windowToEternity
          Left _ -> putStrLn "Exception"
