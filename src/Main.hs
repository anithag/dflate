{-# LANGUAGE TemplateHaskell #-}
module Main where

-- imports for spawning a process on remote node
import Control.Monad (forever, void)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getArgs)
import Foreign.StablePtr

import Data.Map as M
import Data.Set as S
import Data.List as L


import DflateTerm
import Simulator


-- temporary term for testing.
-- replace this with actual parsing of input file
getinput :: Term
getinput = (TEE (Prim (N "local"))  (I 42))

main :: IO ()
main = do
    let lp = "8001"  -- local port on which simulator starts
    let laddr = "10.0.3.1" -- local node's ip address
    let inp = getinput
    Right transport <- createTransport laddr lp (\port'-> (laddr, port') ) defaultTCPParameters
        -- An actual remote node that can be pinged
    let local = mkAddr laddr lp
    putStrLn $ "Starting simulator on  " ++ local
    node <- newLocalNode transport myRemoteTable
    runProcess node $ do
      spid <- getSelfPid
      void $ spawnProcesswith (eval  $ Cfg {term = inp, env = M.empty, place = Prim (N local), pid = spid, chanMap = M.empty})  S.empty
  where
    mkAddr ipaddr port = ipaddr ++ ":" ++ port ++ ":0"
