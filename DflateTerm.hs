{-# LANGUAGE PostfixOperators #-}
module DflateTerm where

import Data.Map as M
import Data.Set as S
import Data.List as L

-- primitive labels
data L =
   N String   -- primitive strings
  |T   -- Top
  |B   -- Bot
  deriving (Eq, Ord)

instance Show L where
  show (N s) = s
  show T = "⊤"
  show B = "⊥"
  
data Principal =
  Prim L
  | (:→)  !Principal
  | (:←) !Principal
  | !Principal :∧ !Principal
  | !Principal :∨ !Principal
--  | T String  --computation principals
  deriving (Ord)

instance Eq Principal where
  (Prim (N s1)) == (Prim (N s2)) = s1 == s2
  (Prim T) == (Prim T) = True
  (Prim B) == (Prim B) = True
  ((:→) p) == (:→) q = p == q
  p == (:→) ((:→) q) = p == q
  (:→) ((:→) p) == q = p == q
  ((:←) p) == (:←) q = p == q
  p == (:←) ((:←) q) = p == q
  (:←) ((:←) p) == q = p == q
   -- include commutativity for ∧
  (p₁ :∧ q₁) == (p₂ :∧ q₂) = ((p₁ == p₂) && (q₁ == q₂)) || ((p₁ == q₂) && (p₂ == q₁))
   -- include commutativity for ∨
  (p₁ :∨ q₁) == (p₂ :∨ q₂) = ((p₁ == p₂) && (q₁ == q₂)) || ((p₁ == q₂) && (p₂ == q₁))
  _ == _ = False

{-
instance Ord Principal where
  compare (Prim s1) (Prim s2) = compare s1 s2
  compare  ((:→) p)  ((:→) q) = compare p q
  compare  ((:←) p)  ((:←) q) = compare p q
  compare  (p₁ :∧ q₁) (p₂ :∧ q₂) = if (p₁ == q₂) && (p₂ == q₁) then EQ
    else if (compare p₁ p₂) == EQ then compare q₁ q₂
    else compare p₁ p₂
  compare  (p₁ :∨ q₁) (p₂ :∨ q₂) = if (p₁ == q₂) && (p₂ == q₁) then EQ
    else if (compare p₁ p₂) == EQ then compare q₁ q₂
    else compare p₁ p₂
-}


instance Show Principal where
  show (Prim  (N p)) = p
  show (Prim T) = "⊤"
  show (Prim B) = "⊥"
  show ((:→) p) = "(" ++ (show p) ++ ")→"
  show ((:←) p) = "(" ++ (show p) ++ ")←"
  show (p1 :∧ p2) = "(" ++ (show p1) ++ " ∧ " ++ (show p2) ++ ")"
  show (p1 :∨ p2) = "(" ++ (show p1) ++ " ∨ " ++ (show p2) ++ ")"
  
type Label = Principal
type PC = Principal
type Place = Principal -- fixme: Use gadt to express primtive principals?

type Delegation = (Principal, Principal)
-- Π = set of delegations
type DelContext = S.Set Delegation 

data DotType =
  Principal :> Principal
  | UnitTy
  | IntTy
  | SumTy DotType DotType
  | ProdTy DotType DotType
  | FunTy DotType PC Theta Type
  | SaysTy Label DotType
  deriving Eq

instance Show DotType where
  show  (p1 :> p2) = (show p1) ++ " ≽ "  ++ (show p2)
  show UnitTy = "()"
  show IntTy = " int "
  show (SumTy ty1 ty2) = (show ty1) ++ "+" ++  (show ty2)
  show (ProdTy ty1 ty2) =  (show ty1) ++ "x" ++  (show ty2)
  show (FunTy ty1 pc theta ty2 ) = (show ty1)  ++ "[" ++  (show pc) ++ ", " ++ (show (Prim B)) ++ "]" ++ (show ty2)
  show (SaysTy l ty) = (show l) ++ (show ty) 
  
data Type = Dot DotType | Halt DotType
  deriving (Eq, Show)

type TypeEnv = M.Map String Type

 
data Channel =
  SendCh Principal Principal PC DotType
  | RecvCh Principal Principal PC DotType
  deriving (Eq, Show)

type Theta = M.Map String Channel 
  
data Term =
  Var String
  | Unit
  | I Integer
  | Actsfor Principal Principal
  | Abs String DotType Principal Theta Term
  | App Term Term
  | Case Term Term Term
  | Pair Term Term
  | Fst Term
  | Snd Term
  | Bind String Term Term
  | Protect Label Term
  | Assume Term Term
  | Send String Term Term
  | Receive String String Term
  | TEE Principal Term -- ensure no recursive TEE
  | RunTEE String Term
  | Term :@ Term
  deriving (Eq, Show)
