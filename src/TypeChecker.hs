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
  (≤) l ty  = do
    del <- getDelContext
    case ty of
      (Dot UnitTy) -> return True
      (Dot (p :> q)) -> return True
      (Dot  (ProdTy ty₁ ty₂)) -> do
        st₁ <- l ≤ (Dot ty₁)
        st₂ <- l ≤ (Dot ty₂)
        return $ st₁ && st₂
      (Dot  (SaysTy l' ty)) -> do
        st₁ <- l ≤ (Dot ty)
        st₂ <- l ⊑ l'
        return $ st₁ || st₂
      (Halt ty) -> l ≤ (Dot ty)
      _ -> return  False
      
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
  -- compute voice
  voice p = let ((:→) pc) :∧  ((:←) pi) = factorize p in
    return $ ((:←) pc) :∧ ((:←) pi)
  
channellookup :: (DFlacMonad m) => Theta -> Channel -> m ChannelTy
channellookup theta ch = case (M.lookup ch theta) of
  Just ty -> return ty
  Nothing -> fail $ "Channel " ++ ch ++ " not found"
  
typecheck :: (DFlacMonad m) => TypeEnv -> Theta -> Place -> PC ->Term -> m Type
typecheck  g theta p pc e = case e of
   Var s -> fail "lookup failed"  
   Unit  -> return (Dot UnitTy)
   I n -> return (Dot IntTy)
   Actsfor p1 p2 -> return (Dot $ p1 :>  p2)
   InjL t (SumTy ty1 ty2) -> do
                    (Dot ty) <- typecheck g theta p pc t
                    cl <- clearance p pc
                    case (cl, ty == ty1) of
                      (True, True) -> return (Dot $ SumTy ty1 ty2 )
                      (_, _ ) -> fail $ "InjL failed to type check"
   InjR t (SumTy ty1 ty2)  -> do
                    (Dot ty) <- typecheck g theta p pc t
                    cl <- clearance p pc
                    case (cl,  ty == ty2) of
                      (True, True) -> return (Dot $ SumTy ty1 ty2)
                      (_, _ ) -> fail $ "InjR failed to type check"
   Abs x ty pc' theta'  t -> do
     rt <- typecheck (M.insert x (Dot ty) g) theta' p pc' t
     cl <- clearance p  pc
     case cl of
       True ->     return (Dot $ FunTy ty pc' theta' rt)
       False -> fail "Clearance failed for Abstraction"
   App t1 t2 -> do
     Dot (FunTy argty pc' theta' rt) <- typecheck g theta p pc t1
     Dot argty' <- typecheck g theta p pc t2
     cl <- clearance p  pc
     flows <- pc ⊑ pc'
     case (argty == argty', cl, flows) of
       (True, True, True) -> return rt
       (_, _,_)  -> fail "App failed to type check"
   Case t1 x t2 y t3 -> do
     Dot (SumTy ty1 ty2) <- typecheck g theta p pc t1
     tyl <- typecheck (M.insert x (Dot ty1) g) theta p pc t2
     tyr <- typecheck (M.insert y (Dot ty2) g) theta p pc t3
     cl <- clearance p pc
     case (cl, tyl == tyr) of
       (True, True) -> do
         status <- pc ≤ tyl
         case status of
           True -> return tyl
           False -> fail $ "Case failed to type check due to insufficient pc protection"
       (_, _) -> fail $ "Case failed to type check. Different types for branches"
   Pair t1 t2 -> do
     (Dot ty1) <- typecheck g theta p pc t1
     (Dot ty2) <- typecheck g theta p pc t2
     cl <- clearance p pc
     case cl of
       True ->      return $ Dot (ProdTy ty1 ty2)
       False -> fail $ "Pair failed to type check"
   Fst t -> do
     Dot (ProdTy ty1 ty2) <-  typecheck g theta p pc t
     cl <- clearance p pc
     case cl of
       True -> return $ Dot ty1
       False -> fail $ "Fst failed to type check"
   Snd t -> do
     Dot (ProdTy ty1 ty2) <-  typecheck g theta p pc t
     cl <- clearance p pc
     case cl of
       True -> return $ Dot ty2
       False -> fail $ "Snd failed to type check"
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
   t :@ v -> return (Dot UnitTy)
   Send ch t1 t2 -> do
     (Dot ty) <- typecheck g M.empty p pc t1
     (SendCh p' q pcs tyc) <- channellookup theta ch
     cl <- clearance p pc
     let pc' = pc
     ty' <- typecheck g theta p pc t2
     status1 <- pc ⊑ pcs
     status2 <- pcs ⊑ pc'
     status3 <- pc' ≤ ty'
     status4 <- p ≽ pcs
     case (cl, status1, status2, status3, status4, (p == p') && (ty == tyc)) of
       (True, True, True, True, True, True) -> case ty' of
         Dot dty' -> return $ Halt dty'
         Halt dty' -> return $ Halt dty'
       (s1, s2, s3, s4, s5, s6) -> fail $ "Send failed to typecheck (clearance, pc ⊑ pc_ch, pc_ch ⊑ pc', pc' ≤ τ', p ≽ pc_ch, channel place and type) :" ++ (show s1) ++ " " ++ (show s1) ++ " "++ (show s2) ++ " "++ (show s3) ++ " "++ (show s4) ++ " "++ (show s5) ++ " "++ (show s6)
       
   Receive ch x t -> do
--     (Dot ty) <- typecheck g M.empty p pc t1
     (RecvCh p' q pcr tyc) <- channellookup theta ch
     cl <- clearance p pc
     let pc' = pc
     ty' <- typecheck (M.insert x (Dot tyc) g) theta p pc t
     status1 <- pc ⊑ pcr
     status2 <- pcr ⊑ pc'
     status3 <- pc' ≤ ty'
     status4 <- p ≽ pcr
     case (cl, status1, status2, status3, status4, (p == p')) of
       (True, True, True, True, True, True) -> case ty' of
         Dot dty' -> return $ Halt dty'
         Halt dty' -> return $ Halt dty'
       (s1, s2, s3, s4, s5, s6) -> fail $ "Receive failed to typecheck (clearance, pc ⊑ pc_ch, pc_ch ⊑ pc', pc' ≤ τ', p ≽ pc_ch, channel place) :" ++ (show s1) ++ " " ++ (show s1) ++ " "++ (show s2) ++ " "++ (show s3) ++ " "++ (show s4) ++ " "++ (show s5) ++ " "++ (show s6)
       
   TEE p t  -> return (Dot UnitTy)
   RunTEE t t' -> return (Dot UnitTy)
   Spawn p' q chr pcr  tyr chs pcs tys t1 t2 -> do
     let pc' = pc
     ty' <- typecheck M.empty (M.insert chr (RecvCh p' p pcr tyr) $ M.insert chs (SendCh p p' pcs tys) M.empty) q pc' t1
     ty <- typecheck M.empty (M.insert chs (SendCh p p' pcs tys) $ M.insert chr (RecvCh p' p pcr tyr) M.empty) p pc t2
     cl <- clearance p pc
     status1 <- pc ⊑ pc'
     status2 <- pc' ≤ ty'
     case (cl, status1, status2, (p == q) || (p' == q)) of
       (True, True, True, True) -> return ty
       (s1, s2, s3, s4) -> fail $ "Spawn failed to type check (clearance, pc ⊑ pc', pc' ≤ τ', p ≠ q ⇒ p' = q) :" ++ (show cl) ++ " " ++ (show status1) ++ " " ++ (show status2) ++ " " ++ (show s4)


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
 
