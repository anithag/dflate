
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Simulator where


import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import DflateTerm

import Data.Map as M
import Data.Set as S
import Data.List as L


-- Serialized version of ThreadCfg.                                                                                                                                                                                                           
-- Used in TEE                                                                                                                                                                                                                                
data SerializeThreadCfg =
  SerializeCfg { sterm :: Term,
			 senv  :: M.Map String Term,
			 splace:: Place,
                 spid :: ProcessId,  -- current process id                                                                                                                                                                                    
                 sppid :: ProcessId, -- parent process id                                                                                                                                                                                     
                 schanMap :: M.Map Channel (SendPort Term)  -- ReceivePort cannot be serialized                                                                                                                                               
  } deriving (Eq, Show, Typeable, Generic)

instance Binary SerializeThreadCfg

-- Almost similar to eval except that there are no user-defined monads involved.                                                                                                                                                              
remoteeval :: SerializeThreadCfg -> Process SerializeThreadCfg
remoteeval cfg = case (sterm cfg) of
  Var x -> let tenv = senv cfg in
    case (M.lookup x tenv) of
      Nothing -> do
        liftIO $ putStrLn   $ "Lookup for variable " ++ x ++ " failed."
        return cfg {  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }
      Just t -> return $ cfg { sterm = t }
  I n -> return cfg
  Unit -> return cfg
  Abs x ty pc' theta'  t ->  return cfg
  Actsfor p q ->  return cfg
  InjL t ty -> do
    cfg' <- remoteeval cfg{sterm = t}
    return cfg{sterm = InjL (sterm cfg') ty }
  InjR t ty -> do
    cfg' <- remoteeval cfg{sterm = t}
    return cfg{sterm = InjR (sterm cfg') ty }
  t1 :@ t2 -> return cfg
  App t1 t2 -> do
    lamcfg <-remoteeval cfg{sterm = t1}
    argcfg <-remoteeval cfg{sterm = t2}
    case (sterm lamcfg) of
      Abs x ty pc' theta' t -> do
        let e' = senv lamcfg
        let v = sterm argcfg
        return lamcfg{sterm = t, senv = M.insert x v e'}
  Case t1 x t2 y t3 -> do
    ccfg <-remoteeval cfg{sterm = t1}
    let e' = senv cfg
    case (sterm ccfg) of
      InjL  v _ ->remoteeval cfg{sterm = t2, senv = M.insert x v e'}
      InjR  v _ ->remoteeval cfg{sterm = t3, senv = M.insert y v e'}
  Pair t1 t2 -> do
    cfg1 <-remoteeval cfg{sterm = t1}
    cfg2 <-remoteeval cfg{sterm = t2}
    return cfg{sterm = Pair (sterm cfg1) (sterm cfg2)}
  Fst t -> do
    cfg' <-remoteeval cfg{sterm = t}
    case (sterm cfg') of
      Pair v1 v2 -> return cfg{sterm =v1}
      _ -> do
        liftIO $ putStrLn   $ "Expected a pair value, but received none"
        return cfg{  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }
  Snd t -> do
    cfg' <-remoteeval cfg{sterm = t}
    case (sterm cfg') of
      Pair v1 v2 -> return cfg{sterm =v2}
      _ -> do
        liftIO $ putStrLn   $ "Expected a pair value, but received none"
        return cfg{  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }
  Bind x t1 t2  -> do
    cfg1 <-remoteeval cfg{sterm = t1}
    let e' = senv cfg
    case (sterm cfg1) of
      Protect l t -> return cfg{sterm = t2, senv = M.insert x t e'}
      _ -> do
        liftIO $ putStrLn   $ "Expected a protected value, received none"
        return cfg{  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }
  Protect l t  -> do
     cfg' <-remoteeval cfg{sterm = t}
     return cfg{sterm = Protect l (sterm cfg')}
  Assume t1 t2 -> do
    cfg1 <-remoteeval cfg{sterm = t1}
    let v = sterm cfg1  -- can be acts-for or where term                                                                                                                                                                                      
    return cfg{sterm = t2 :@ v}
  Send ch t1 t2 ->
    let chmap = schanMap cfg in
    case (M.lookup ch chmap) of
      Just ch' -> do
        cfg' <- remoteeval cfg{sterm = t1}
        -- get the value                                                                                                                                                                                                                      
        let v = sterm cfg'
        -- send the value to the channel                                                                                                                                                                                                      
        sendChan ch' v
        -- continue with continuation                                                                                                                                                                                                         
        return cfg{sterm = t2}
      Nothing -> do
        liftIO $ putStrLn   $ "Channel not found in the environment"
        return cfg{  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }
  _ ->  do
    liftIO $ putStrLn   $ "unhandled case"
    return cfg{  sterm = Unit,  senv = M.empty,  splace = Prim B,  schanMap = M.empty  }

test :: SerializeThreadCfg -> Process ()
test cfg =
  let pid = spid cfg in -- get the thread to respond to                                                                                                                                                                                       
  do
    (chs, chr) <- newChan :: Process (SendPort Term, ReceivePort Term) -- typed channels 
    send pid (chs::(SendPort Term))
    cfg' <- (remoteeval cfg)
    liftIO $ putStrLn $ "evaluation complete."
    --send pid ((sterm cfg') :: Term)

remotable['test]

myRemoteTable :: RemoteTable
myRemoteTable = Simulator.__remoteTable initRemoteTable
