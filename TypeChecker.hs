{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
module TypeChecker where

import Control.Monad.State.Lazy
import Data.Map as M
import Data.Set as S
import DflateTerm
import FlacMonad



newtype DflateM a = DflateM { unDflateM :: StateT DelContext IO a  }
  deriving (Functor, Applicative, Monad, MonadState DelContext, MonadIO)

instance DFlacMonad DflateM where
--  getDelContext = get
  -- Π, ⊥, ⊤  ⊩ p₁ ≽ p₂
  (≽) p1 p2 = return True
  -- Π, ⊥, ⊤  ⊩ ℓ ≤ τ
  (≤) l ty  = return True
  -- Π, ⊥, ⊤  ⊩ p₁ ⊑ p₂
  (⊑) p1 p2 = return  True
  -- Π, ⊥, ⊤  ⊩ p₁ ⊔ p₂
  (⊔) p1 p2 = return (Prim T)
  -- Π, ⊥, ⊤  ⊩ p₁ ⊓ p₂
  (⊓) p1 p2 = return (Prim B)
  -- clearance
  clearance p pc = p ≽ pc
  getState = get
  -- compute voice
  voice p = return (Prim B)


typecheck :: (DFlacMonad m) => DelContext -> TypeEnv -> Theta -> Place -> PC ->Term -> m Type
typecheck pi g theta p pc e = case e of
   Var s -> fail "lookup failed"  
   Unit  -> return (Dot UnitTy)
   I n -> return (Dot IntTy)
   Actsfor p1 p2 -> return (Dot $ p1 :>  p2)
   Abs x ty pc' theta'  t -> do
     rt <- typecheck pi (M.insert x (Dot ty) g) theta' p pc' t
     cl <- clearance p  pc
     case cl of
       True ->     return (Dot $ FunTy ty pc' theta' rt)
       False -> fail "Clearance failed for Abstraction"
   App t1 t2 -> do
     Dot (FunTy argty pc' theta' rt) <- typecheck pi g theta p pc t1
     Dot argty' <- typecheck pi g theta p pc' t2
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
     Dot (SaysTy l ty) <- typecheck pi g theta p pc t1
     j <- (pc ⊔ l)
     ty' <- typecheck pi (M.insert x (Dot ty) g) theta p j t2
     return ty'
   Protect l t  -> do
     Dot ty <- typecheck pi g theta p pc t
     status <- pc ⊑ l
     cl <- clearance p pc
     case (status, cl) of
       (True, True) -> return (Dot (SaysTy l ty))
       (_, _) -> fail "Protects failed to type check"
   Assume t1 t2 -> do
     Dot (p' :> q) <- typecheck pi g theta p pc t1
     cl <- clearance p pc
     vp' <- voice p'
     vq <- voice q
     status1 <- pc ≽ vq
     status2 <- vp' ≽ vq
     ty <- typecheck (S.insert (p', q) pi) g theta p pc t2
     case (cl, status1, status2) of
       (True, True, True) -> return ty
       (_, _, _) -> fail " Assume failed to typecheck"
   Send c t1 t2 -> return (Dot UnitTy)
   Receive c x t -> return (Dot UnitTy)
   TEE p t  -> return (Dot UnitTy)
   RunTEE t t' -> return (Dot UnitTy)
   t :@ v -> return (Dot UnitTy)


test :: DflateM Type
test = typecheck  S.empty M.empty M.empty (Prim B) (Prim B) (Var "x")

test1 :: (DFlacMonad m) => m Type
test1 = typecheck  S.empty M.empty M.empty (Prim B) (Prim B) Unit

runtc :: IO ()
runtc = do
  t <- runStateT  (unDflateM test) S.empty
  case (fst t) of
    Dot t -> putStrLn $ show t
    _ -> fail $ "Unknown error"
 
