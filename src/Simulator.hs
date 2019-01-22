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
import qualified Data.Binary as B
import Data.Typeable
import GHC.Generics (Generic)

import qualified Control.Concurrent as C

-- imports for spawning a process on remote node
import Control.Monad (forever)
import Control.Distributed.Process

import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getArgs)
import Foreign.StablePtr
import System.LXC

class (Monad m, MonadIO m) => SimulatorM m where
  getDelContext :: m DelContext
  putDelContext :: DelContext -> m ()
  freshName :: m String
  liftProcess :: Process a -> m a
  getPid :: m ProcessId

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
  getPid = liftProcess $ getSelfPid

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

instance B.Binary SerializeThreadCfg

  
-- used in the evaluation by remote process.
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
  Unit -> return cfg
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

      do
        del <- getDelContext
        let chmap = chanMap cfg
        (chs', chr') <- liftProcess  newChan  -- send and receive channel (for spawned process)
        spid <- liftProcess $ spawnLocal $ do
          spid <- getSelfPid
          void $ spawnProcesswith (eval  $ Cfg {term = t1, env = M.empty, place = q,  pid = spid, chanMap = M.fromList [(chs, SendTy chs'), (chr, ReceiveTy chr')]})  del
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
    do
      -- start the worker node on the container
      -- installContainer -- disabled for now
      {-
      err <- withContainer (Container "my-container" Nothing) $ do
        attachRunWait defaultAttachOptions{attachUID = 0} "/home/anitha/tee/worker" ["worker", "worker", "8080"]
            -- FIXME: ReceivePort cannot be serialized
           {-
                 Parent [chr'] <---- [chs'] Child
                 Parent [chs''] -----> [chr''] Child  
          -}
      -}
      let lp = "8081"
      let rp = "8080"
      let laddr = "10.0.3.1"
      let raddr = "10.0.3.6"
      let qs = (mkTEEChannel q) ++ "send"
      let qr = (mkTEEChannel q) ++ "recv"
      let chmap = chanMap cfg
      Right transport <- liftIO $ createTransport "10.0.3.1" lp (\port'-> ("10.0.3.1", port') ) defaultTCPParameters
      let remote = mkAddr raddr rp
      let them = NodeId $ EndPointAddress (BS8.pack remote)
      liftIO $ putStrLn "Starting enclave ..."
      node <- liftIO $ newLocalNode transport myRemoteTable
      reply <- liftIO $ C.newEmptyMVar
      ppid <- getPid
      (chs, chr) <- liftProcess $ newChan -- typed channels
      tmpid <- liftIO $ forkProcess node $ do
        thispid <- getSelfPid
        liftIO $ putStrLn "Calling enclave ..."
--        _ <- spawn them $ $(mkClosure 'test) (SerializeCfg{sterm = t, senv = M.empty, splace = q, spid = thispid, sppid = ppid, schanMap = M.empty })
        -- RPC hangs. Fix later
        res <- call  $(functionTDict 'test) them $ $(mkClosure 'test)  (SerializeCfg{sterm = t, senv = M.empty, splace = q, spid = thispid, sppid = ppid, schanMap = M.union (M.fromList [(qr, chs)]) M.empty})
        liftIO $ putStrLn $ "Waiting for reply from enclave ..."
        sendport <- expect :: Process (SendPort Term)
        send ppid sendport -- send to host process
        liftIO $ print  "RPC reply : "
        liftIO $ C.putMVar reply (show res)
        liftIO $ print =<< C.takeMVar reply
      -- monitor the forked process?
      mref <- liftProcess $ monitor tmpid
      sendport <- liftProcess $ do
        sendport <- expect :: Process (SendPort Term)
        return sendport
      liftIO $ putStrLn $ "Received enclave's send port " ++ (show sendport)
      return cfg{ term = RunTEE q Unit, chanMap = M.union (M.fromList [(qr, ReceiveTy chr), (qs, SendTy sendport)]) chmap  }
  _ -> fail  "Case not handled"


mkAddr :: String -> String -> String
mkAddr ipaddr port = ipaddr ++ ":" ++ port ++ ":0"

installContainer :: IO ()
installContainer = withContainer (Container "my-container" Nothing) $ do
  -- expensive process;
  status <- create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "bionic", "-a", "amd64"]
  start False []
  let attachopt = defaultAttachOptions{attachUID = 0}
  attachRunWait  attachopt "apt-get" ["apt-get", "install", "ghc"]
  attachRunWait  attachopt "apt-get" ["apt-get", "install", "cabal-install"]
  attachRunWait  attachopt "cabal" ["cabal", "update"]
  attachRunWait  attachopt "cabal" ["sudo cabal", "install", "distributed-process", "network-transport-tcp"]
  attachRunWait  attachopt "/home/anitha/tee/worker" ["worker", "worker", "8080"]
  void $ stop

