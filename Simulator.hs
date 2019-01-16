{-# language GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# language DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Simulator where

import DflateTerm hiding (get)
import Control.Concurrent
import Data.Map as M
import Data.List as L
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Concurrent.Chan


-- imports for spawning a process on remote node
import Control.Monad (forever)
import qualified Control.Distributed.Process as DP
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

-- Monad to generate fresh names  
newtype FreshT m b =
  FreshT { unFreshT :: StateT Int m b }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class (Monad m) => MonadFresh m where
  fresh :: m String

alphaStar :: [String]
alphaStar = L.map (:[]) alpha ++ [ a : as | as <- alphaStar, a <- alpha ]
            where alpha = ['a'..'z']


freshPrim_ :: MonadState Int m => m String
freshPrim_ =
  do n <- get
     modify (+1)
     return (alphaStar !! n)

-- FreshT is an instance of MonadFresh
instance Monad m => MonadFresh (FreshT m) where
  fresh =
    FreshT freshPrim_

runFreshT :: Monad m => FreshT m b -> m b
runFreshT m = fst <$> runStateT (unFreshT m) 0


newtype DflateSim a = DflateSim { unDflateSim :: StateT DelContext (FreshT IO) a  }
  deriving (Functor, Applicative, Monad, MonadState DelContext, MonadIO)

instance SimulatorM DflateSim where
  getDelContext = get
  putDelContext = put
  freshName = DflateSim (lift fresh)

data ThreadCfg =
  Cfg { term :: Term,
        env  :: M.Map String Term,
        place:: Place,
        threadId :: String,
        chanMap :: M.Map Channel (Chan Term) -- map from Dflate Channel type to Control.Concurrent.Chan
  }


spawnThreadwith :: DflateSim a -> DelContext -> IO (a, DelContext)
spawnThreadwith m d  = runFreshT $ runStateT (unDflateSim m) d

chanlookup :: (SimulatorM m) => Channel -> M.Map Channel (Chan Term) -> m (Chan Term)
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
    let chr' = newChan in
      do
        del <- getDelContext
        let chmap = chanMap cfg
        chr' <- liftIO $ newChan  -- receive channel (for spawned process)
        chs' <- liftIO $ newChan  -- send channel (for spawned process) 
        tid <- liftIO $ forkIO(do
                                  spawnThreadwith (eval  $ Cfg {term = t1, env = M.empty, place = q,  threadId = "test", chanMap = M.fromList [(chr, chr'), (chs, chs')]})  del
                                  putStrLn $ "Spawned a computation on node " ++ (show q))
        return cfg{ term = t2, chanMap = M.union (M.fromList [(chr, chr'), (chs, chs')]) chmap}
  Send ch t1 t2 -> do
    let chmap = chanMap cfg
    ch' <- chanlookup ch chmap
    cfg' <- eval cfg{term = t1}
    -- get the value
    let v = term cfg'
    -- write the value to the channel
    -- Fixme: This should be synchronous
    liftIO $ writeChan ch' v
    -- continue with continuation
    return cfg{term = t2}
  Receive ch x t -> do
    let chmap = chanMap cfg
    let e' = env cfg
    ch' <- chanlookup ch chmap
    -- read the value from the channel
    -- Fixme: This should be synchronous
    v <- liftIO $ readChan ch'
    -- continue with continuation
    return cfg{term = t, env = M.insert x v e'}
  TEE p t -> fail  "Support in progress"
  _ -> fail  "Case not handled"
  
