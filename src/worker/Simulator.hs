
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
remoteeval :: ThreadCfg -> Process ThreadCfg
remoteeval cfg = case (term cfg) of
  Var x -> let tenv = env cfg in
    case (M.lookup x tenv) of
      Nothing -> do
        liftIO $ putStrLn   $ "Lookup for variable " ++ x ++ " failed."
        return cfg {  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  }
      Just t -> return $ cfg { term = t }
  I n -> return cfg
  Unit -> return cfg
  Abs x ty pc' theta'  t ->  return cfg
  Actsfor p q ->  return cfg
  InjL t ty -> do
    cfg' <- remoteeval cfg{term = t}
    return cfg{term = InjL (term cfg') ty }
  InjR t ty -> do
    cfg' <- remoteeval cfg{term = t}
    return cfg{term = InjR (term cfg') ty } 
  t1 :@ t2 -> return cfg
  App t1 t2 -> do
    lamcfg <-remoteeval cfg{term = t1}
    argcfg <-remoteeval cfg{term = t2}
    case (term lamcfg) of
      Abs x ty pc' theta' t -> do
        let e' = env lamcfg
        let v = term argcfg
        return lamcfg{term = t, env = M.insert x v e'}
  Case t1 x t2 y t3 -> do
    ccfg <-remoteeval cfg{term = t1}
    let e' = env cfg
    case (term ccfg) of
      InjL  v _ ->remoteeval cfg{term = t2, env = M.insert x v e'}
      InjR  v _ ->remoteeval cfg{term = t3, env = M.insert y v e'}
  Pair t1 t2 -> do
    cfg1 <-remoteeval cfg{term = t1}
    cfg2 <-remoteeval cfg{term = t2}
    return cfg{term = Pair (term cfg1) (term cfg2)}
  Fst t -> do
    cfg' <-remoteeval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v1}
      _ -> do
        liftIO $ putStrLn   $ "Expected a pair value, but received none"
        return cfg{  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  }
  Snd t -> do
    cfg' <-remoteeval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v2}
      _ -> do
        liftIO $ putStrLn   $ "Expected a pair value, but received none"
        return cfg{  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  } 
  Bind x t1 t2  -> do
    cfg1 <-remoteeval cfg{term = t1}
    let e' = env cfg
    case (term cfg1) of
      Protect l t -> return cfg{term = t2, env = M.insert x t e'}
      _ -> do
        liftIO $ putStrLn   $ "Expected a protected value, received none"
        return cfg{  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  }  
  Protect l t  -> do
     cfg' <-remoteeval cfg{term = t}
     return cfg{term = Protect l (term cfg')}
  Assume t1 t2 -> do
    cfg1 <-remoteeval cfg{term = t1}
    let v = term cfg1  -- can be acts-for or where term
    return cfg{term = t2 :@ v}
  Send ch t1 t2 ->
    let chmap = chanMap cfg in
    case (M.lookup ch chmap) of
      Just (SendTy ch') -> do
        cfg' <- remoteeval cfg{term = t1}
        -- get the value
        let v = term cfg'
        -- send the value to the channel
        sendChan ch' v
        -- continue with continuation
        return cfg{term = t2}
      Nothing -> do
        liftIO $ putStrLn   $ "Channel not found in the environment"
        return cfg{  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  }  
  _ ->  do
    liftIO $ putStrLn   $ "unhandled case"
    return cfg{  term = Unit,  env = M.empty,  place = Prim B,  chanMap = M.empty  } 

   

convertSerializeThreadCfg :: SerializeThreadCfg -> ThreadCfg
convertSerializeThreadCfg scfg =
  Cfg { term = sterm scfg, env = senv scfg, place = splace scfg, pid = spid scfg,  chanMap = M.fromList (L.map (\(a, b) -> (a, SendTy b)) (M.toList   (schanMap scfg))) }

mkTEEChannel :: Principal -> String
mkTEEChannel (Prim (N s)) = s

test :: SerializeThreadCfg -> Process ()
test scfg =
  let pid = spid scfg in -- get the thread to respond to
  do
    (chs, chr) <- newChan -- typed channels
    send pid (chs :: (SendPort Term))
    let cfg = convertSerializeThreadCfg scfg 
    cfg' <- (remoteeval cfg{chanMap = M.union (M.fromList [((mkTEEChannel (place cfg)) , (ReceiveTy chr))]) (chanMap cfg) })
    liftIO $ putStrLn $ "evaluation complete."
    --send pid ((sterm cfg') :: Term)


remotable ['test]

myRemoteTable :: RemoteTable
myRemoteTable = Simulator.__remoteTable initRemoteTable
