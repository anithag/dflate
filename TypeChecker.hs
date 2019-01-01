{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
module TypeChecker where

import Control.Monad.State.Lazy
import Data.Map as M
import Data.Set as S
import DflateTerm
import FlacMonad
import Norm


newtype DflateM a = DflateM { unDflateM :: StateT DelContext IO a  }
  deriving (Functor, Applicative, Monad, MonadState DelContext, MonadIO)

instance DFlacMonad DflateM where
  getDelContext = get
  putDelContext = put
  -- Π, ⊥, ⊤  ⊩ p₁ ≽ p₂
  (≽) p1 p2 = do
    del <- getDelContext
    return (proofsearch del p1 p2)  -- offload proof search to norm module
  -- Π, ⊥, ⊤  ⊩ ℓ ≤ τ
  (≤) l ty  = return True
  -- Π, ⊥, ⊤  ⊩ p₁ ⊑ p₂
  (⊑) p1 p2 = do
    status <- (((:←) p1) :∧ ((:→) p2)) ≽ (((:←) p2) :∧ ((:→) p1))
    return status
  -- Π, ⊥, ⊤  ⊩ p₁ ⊔ p₂
  (⊔) p q = return $ ((:→) (p :∧ q)) :∧ ((:←) (p :∨ q))
  -- Π, ⊥, ⊤  ⊩ p₁ ⊓ p₂
  (⊓) p q = return $ ((:→) (p :∨ q)) :∧ ((:←) (p :∧ q))
  -- clearance
  clearance p pc = p ≽ pc
  getState = get
  -- compute voice
  voice p = let ((:→) pc) :∧  ((:←) pi) = factorize p in
    return $ ((:←) pc) :∧ ((:←) pi)
  

typecheck :: (DFlacMonad m) => TypeEnv -> Theta -> Place -> PC ->Term -> m Type
typecheck  g theta p pc e = case e of
   Var s -> fail "lookup failed"  
   Unit  -> return (Dot UnitTy)
   I n -> return (Dot IntTy)
   Actsfor p1 p2 -> return (Dot $ p1 :>  p2)
   Abs x ty pc' theta'  t -> do
     rt <- typecheck (M.insert x (Dot ty) g) theta' p pc' t
     cl <- clearance p  pc
     case cl of
       True ->     return (Dot $ FunTy ty pc' theta' rt)
       False -> fail "Clearance failed for Abstraction"
   App t1 t2 -> do
     Dot (FunTy argty pc' theta' rt) <- typecheck g theta p pc t1
     Dot argty' <- typecheck g theta p pc' t2
     cl <- clearance p  pc
     flows <- pc ⊑ pc'
     case (argty == argty', cl, flows) of
       (True, True, True) -> return rt
       (_, _,_)  -> fail "App failed to type check"
   Case t1 t2 t3 -> return (Dot UnitTy)
   Pair t1 t2 -> return (Dot UnitTy)
   Fst t -> return (Dot UnitTy)
   Snd t -> return (Dot UnitTy)
   Bind x t1 t2  -> do
     Dot (SaysTy l ty) <- typecheck g theta p pc t1
     j <- (pc ⊔ l)
     ty' <- typecheck (M.insert x (Dot ty) g) theta p j t2
     return ty'
   Protect l t  -> do
     Dot ty <- typecheck g theta p pc t
     status <- pc ⊑ l
     cl <- clearance p pc
     case (status, cl) of
       (True, True) -> return (Dot (SaysTy l ty))
       (_, _) -> fail "Protects failed to type check"
   Assume t1 t2 -> do
     Dot (p' :> q) <- typecheck g theta p pc t1
     cl <- clearance p pc
     vp' <- voice p'
     liftIO $ putStrLn $ "voice of " ++ (show p') ++ " = " ++ (show vp')
     vq <- voice q
     liftIO $ putStrLn $ "voice of " ++ (show q) ++ " = " ++ (show vq)
     status1 <- pc ≽ vq  -- Π ⊩ pc ≽ ∇(p')
     status2 <- vp' ≽ vq  -- Π ⊩ ∇(p') ≽ ∇(q)
     pi <- getDelContext
     putDelContext (S.insert (p', q) pi)  -- Update the delegations
     ty <- typecheck g theta p pc t2
     case (cl, status1, status2) of
       (True, True, True) -> return ty
       (_, _, _) -> fail $ " Assume failed to typecheck (clearance, voice of pc, voice of principals) : " ++ (show cl) ++ " " ++ (show status1) ++ " " ++ (show status2)
   Send c t1 t2 -> return (Dot UnitTy)
   Receive c x t -> return (Dot UnitTy)
   TEE p t  -> return (Dot UnitTy)
   RunTEE t t' -> return (Dot UnitTy)
   t :@ v -> return (Dot UnitTy)


testtc :: DflateM Type
testtc = typecheck  M.empty M.empty (Prim B) (Prim B) (Var "x")

testtc1 :: DflateM Type
testtc1 = typecheck  M.empty M.empty (Prim B) (Prim B) Unit


testtc2 :: DflateM Type
testtc2 = typecheck  M.empty M.empty (Prim (N "q")) (Prim (N "q")) (Assume (Actsfor ((:←)  (Prim (N "p"))) ((:←)  (Prim (N "q")))) (I 10))


runtc :: IO ()
runtc = do
  t <- runStateT  (unDflateM testtc2) $ S.fromList [(((:←) $ Prim (N "p")), ((:←) $ Prim (N "q")))]
  case (fst t) of
    Dot t -> putStrLn $ show t
    _ -> fail $ "Unknown error"
 
