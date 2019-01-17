{-# language GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# language DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Simulator where

import DflateTerm hiding (get)
import Data.Map as M
import Data.List as L
import Control.Monad.Trans
import Control.Monad.State.Lazy


-- imports for spawning a process on remote node
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getArgs)
import Foreign.StablePtr


class (Monad m, MonadIO m) => SimulatorM m where
  getDelContext :: m DelContext
  putDelContext :: DelContext -> m ()
  freshName :: m String
  liftProcess :: Process a -> m a

-- Monad to generate fresh names  
newtype FreshT m b =
  FreshT { unFreshT :: StateT Int m b }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)


alphaStar :: [String]
alphaStar = L.map (:[]) alpha ++ [ a : as | as <- alphaStar, a <- alpha ]
            where alpha = ['a'..'z']


freshPrim_ :: MonadState Int m => m String
freshPrim_ =
  do n <- get
     modify (+1)
     return (alphaStar !! n)

-- FreshT is an instance of MonadFresh
class (Monad m) => MonadFresh m where
  fresh :: m String

instance Monad m => MonadFresh (FreshT m) where
  fresh =
    FreshT freshPrim_

runFreshT :: Monad m => FreshT m b -> m b
runFreshT m = fst <$> runStateT (unFreshT m) 0


newtype DflateSim a = DflateSim { unDflateSim :: StateT DelContext (FreshT Process) a  }
  deriving (Functor, Applicative, Monad, MonadState DelContext, MonadIO)

instance SimulatorM DflateSim where
  getDelContext = get
  putDelContext = put
  freshName = DflateSim (lift fresh)
  liftProcess p = DflateSim (lift  (lift  p))

-- Represent process's typed channels
-- Not to be confused with Dflate's typed channels used in type checking
data TypedChannel t =
  SendTy (SendPort t)
  | ReceiveTy (ReceivePort t)
  
data ThreadCfg =
  Cfg { term :: Term,
        env  :: M.Map String Term,
        place:: Place,
        pid :: ProcessId,
        chanMap :: M.Map Channel (TypedChannel Term)  -- map from Dflate Channel type to Control.Concurrent.Chan
  }


spawnProcesswith :: DflateSim a -> DelContext -> Process (a, DelContext)
spawnProcesswith m d  = runFreshT $ runStateT (unDflateSim m) d

chanlookup :: (SimulatorM m) => Channel -> M.Map Channel (TypedChannel Term) -> m (TypedChannel Term)
chanlookup c chmap = case (M.lookup c chmap) of
  Nothing -> fail $ "Channel " ++ c ++ " not found"
  Just ch -> return ch
  
eval :: (SimulatorM m) => ThreadCfg -> m ThreadCfg
eval cfg = case (term cfg) of
  Var x -> let tenv = env cfg in
    case (M.lookup x tenv) of
      Nothing -> fail $ "Lookup for variable " ++ x ++ " failed."
      Just t -> return $ cfg { term = t }
  I n -> return cfg
  Abs x ty pc' theta'  t -> return cfg
  Actsfor p q -> return cfg
  InjL t ty -> do
    cfg' <- eval cfg{term = t}
    return cfg{term = InjL (term cfg') ty }
  InjR t ty -> do
    cfg' <- eval cfg{term = t}
    return cfg{term = InjR (term cfg') ty } 
  t1 :@ t2 -> return cfg   -- where term
  App t1 t2 -> do
    lamcfg <- eval cfg{term = t1}
    argcfg <- eval cfg{term = t2}
    case (term lamcfg) of
      Abs x ty pc' theta' t -> do
        let e' = env lamcfg
        let v = term argcfg
        return lamcfg{term = t, env = M.insert x v e'}
  Case t1 x t2 y t3 -> do
    ccfg <- eval cfg{term = t1}
    let e' = env cfg
    case (term ccfg) of
      InjL  v _ -> eval cfg{term = t2, env = M.insert x v e'}
      InjR  v _ -> eval cfg{term = t3, env = M.insert y v e'}
  Pair t1 t2 -> do
    cfg1 <- eval cfg{term = t1}
    cfg2 <- eval cfg{term = t2}
    return cfg{term = Pair (term cfg1) (term cfg2)}
  Fst t -> do
    cfg' <- eval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v1}
      _ -> fail $ "Expected a pair value, but received none"
  Snd t -> do
    cfg' <- eval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v2}
      _ -> fail $ "Expected a pair value, but received none"
  Bind x t1 t2  -> do
    cfg1 <- eval cfg{term = t1}
    let e' = env cfg
    case (term cfg1) of
      Protect l t -> return cfg{term = t2, env = M.insert x t e'}
      _ -> fail $ "Expected a protected value, received none"
  Protect l t  -> do
     cfg' <- eval cfg{term = t}
     return cfg{term = Protect l (term cfg')}
  Assume t1 t2 -> do
    cfg1 <- eval cfg{term = t1}
    let v = term cfg1  -- can be acts-for or where term
    return cfg{term = t2 :@ v}
  Spawn p' q chr pcr  tyr chs pcs tys t1 t2 ->
    -- spawn a local process for now
    -- using typed channel
    -- FIXME: ReceivePort cannot be serialized
      do
        del <- getDelContext
        let chmap = chanMap cfg
        (chs', chr') <- liftProcess  newChan  -- send and receive channel (for spawned process)
        spid <- liftProcess $ spawnLocal $ do
          spid <- getSelfPid
          void $ spawnProcesswith (eval  $ Cfg {term = t1, env = M.empty, place = q,  pid = spid, chanMap = M.fromList [(chr, ReceiveTy chr'), (chs, SendTy chs')]})  del
          liftIO $ putStrLn $ "Spawned a computation on node " ++ (show q)
        return cfg{ term = t2, chanMap = M.union (M.fromList [(chr, ReceiveTy chr'), (chs, SendTy chs')]) chmap}
  Send ch t1 t2 -> do
    let chmap = chanMap cfg
    SendTy ch' <- chanlookup ch chmap
    cfg' <- eval cfg{term = t1}
    -- get the value
    let v = term cfg'
    -- send the value to the channel
    -- Fixme: This should be synchronous
    liftProcess $ sendChan ch' v
    -- continue with continuation
    return cfg{term = t2}
  Receive ch x t -> do
    let chmap = chanMap cfg
    let e' = env cfg
    ReceiveTy ch' <- chanlookup ch chmap
    -- receive the value from the channel
    -- Fixme: This should be synchronous
    v <- liftProcess $ receiveChan ch'
    -- continue with continuation
    return cfg{term = t, env = M.insert x v e'}
  TEE q t ->
    -- RPC call
    -- similar to spawn for now except that no new channels are created
    -- wait for result
    -- TEE cannot communicate even with the host
      do
        del <- getDelContext
        let chmap = chanMap cfg
        tres <- liftProcess $ callLocal $ do  -- wait for the process to finish. RPC call!
          spid <- getSelfPid
          spawnProcesswith  (eval  $ Cfg {term = t, env = M.empty, place = q,  pid = spid, chanMap = M.empty})  del
        return cfg{ term = RunTEE q Unit} 
    
  _ -> fail  "Case not handled"
  
