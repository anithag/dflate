{-# language GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# language DeriveFunctor #-}
module Simulator where

import DflateTerm
import Control.Concurrent
import Data.Map as M
import Data.List as L
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Concurrent.Chan

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
  App t1 t2 -> do
    lamcfg <- eval cfg{term = t1}
    argcfg <- eval cfg{term = t2}
    case (term lamcfg) of
      Abs x ty pc' theta' t -> do
        let e' = env lamcfg
        let v = term argcfg
        return lamcfg{term = t, env = M.insert x v e'}
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
                       
  _ -> fail $ "Case not handled"
  
