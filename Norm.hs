{-# LANGUAGE PostfixOperators #-}
module Norm where

import DflateTerm
import Data.Set as S
import Data.List as L

newtype M = M {unM :: S.Set L } deriving (Eq, Ord) -- set of ∨ of primitive principals   
newtype J =  J {unJ :: S.Set M } deriving (Eq,  Ord) -- set of ∧ of (∨ of primitive principals)
data NF = NF { confidentiality :: J, integrity :: J } deriving (Eq, Show, Ord)

  
instance Show M where
  show (M m) = show m
  
instance Show J where
  show (J j) = show j

-- {L1, L2 ...} -> { L1 ∨ L2 ∨ ... }
convertM :: M -> Principal
convertM (M m) = L.foldl1 (\x l -> x :∨ l)  (L.map  (\x -> Prim x) (S.toList m))

conjplist :: [Principal] -> Principal
conjplist l = if (L.null l) then Prim B
  else foldl1 (\p l -> p :∧ l) l
  
-- {M1, M2, ...} -> { M1 ∧ M2 ∧ ...}
convertJ :: J -> Principal
convertJ (J j) = let p = L.map  (\x -> (convertM x)) (S.toList j) in
  conjplist p

--  Input : { ({L1, L2}, {L3 , L4} , ...)->, ({L1, L6}, {L7, L9} ...)<-} 
--  Output: { ({L1 ∨ L2} ∧ {L3 ∨ L4} ∧ ...)->, ({L1 ∨ L6} ∧ {L7 ∨ L9} ...)<-} 
convert :: NF -> Principal
convert (NF nc  ni) = let pc = convertJ nc in
  let pi = convertJ ni in
    (pc :→) :∧ (pi :←)
    

-- 1. p ∨ ⊤ = p
-- 2. p ∨ ⊥ = ⊥
simplifyOrM :: S.Set L -> S.Set L
simplifyOrM m = S.foldl (\a b -> case b of
                          B -> S.singleton B
                          T -> if (S.null a) then (S.singleton T)  -- a could be emoty in which case return T
                            else a
                          N p -> if a == S.singleton B then a
                            else
                            if a ==  S.singleton T then S.singleton b
                            else  S.union a (S.singleton b)) (S.empty) m
              

-- placeholder to implement more optimizations  
simplifyM :: S.Set L -> S.Set L
simplifyM = simplifyOrM

-- 1. p ∧ ⊤ = ⊤
-- 2. p ∧ ⊥ = p
foldlfunc :: S.Set M -> M -> S.Set M
foldlfunc a b =
  -- check if b = ⊥
  if b == (M (S.singleton B)) then a
  else
    -- check if b = ⊤
    if b == (M (S.singleton T)) then (S.singleton b)
    else
      -- check if a = ⊥
      if a == (S.singleton $ M (S.singleton B)) then (S.singleton b)
      else
        -- check if a = ⊤
        if a  == (S.singleton $ M (S.singleton T)) then a
        -- neither a nor b are special principals
        else  S.union a (S.singleton b)

          
simplifyAndJ :: S.Set M ->  S.Set M
simplifyAndJ j =
  let j' = S.map (\(M l)-> M (simplifyM l)) j in
    let j'' = S.filter (\(M l) -> if (S.null l) then False
                         else True) j' in
      let j''' = S.foldl  foldlfunc  S.empty j'' in
        j'''


simplifyJ :: S.Set M -> S.Set M
simplifyJ = simplifyAndJ

simplify :: NF -> NF
simplify (NF (J nc) (J ni)) = NF { confidentiality = J (simplifyJ nc),
                                   integrity = J (simplifyJ ni)}
normalize :: Principal -> NF
normalize (Prim T)  = (NF { confidentiality = J $ S.singleton (M $ S.singleton T),
                            integrity = J $ S.singleton (M $ S.singleton T)})

normalize (Prim B)  = (NF { confidentiality = J $ S.singleton (M $ S.singleton B),
                            integrity = J $ S.singleton (M $ S.singleton B)})

normalize (Prim (N p))  = (NF { confidentiality = J $ S.singleton (M $ S.singleton (N p)),
                            integrity = J $ S.singleton (M $ S.singleton (N p))})

normalize (p1 :∧ p2) =
  let (NF (J nc1) (J ni1)) = (normalize p1) in
    let (NF (J nc2) (J ni2)) = (normalize p2) in
      let r = NF { confidentiality = J $ S.union nc1 nc2,
                   integrity = J $ S.union ni1 ni2 } in
        r


normalize (p1 :∨ p2) =
  let (NF (J nc1) (J ni1)) = (normalize p1) in
    let (NF (J nc2) (J ni2)) = (normalize p2) in
      let nc' = S.fromList $ concatMap (zipWith (\(M m1) (M m2) -> M (S.union m1 m2)) (S.toList nc1) . repeat) (S.toList nc2) in
        let ni' = S.fromList $ concatMap (zipWith (\(M m1) (M m2) -> M (S.union m1 m2)) (S.toList ni1) . repeat) (S.toList ni2) in
          let r = NF { confidentiality = J nc',
                       integrity = J ni' } in
            r

normalize ((:→) p) =
  let (NF (J nc) (J ni)) = normalize p in
    NF { confidentiality = J nc, integrity = J $ S.singleton ( M $ S.singleton B)}

normalize ((:←) p) =
  let (NF (J nc) (J ni)) = normalize p in
    NF { confidentiality = J $ S.singleton $ M $ S.singleton  B, integrity = J ni }


factorize :: Principal -> Principal  
factorize = convert . simplify. normalize



-- tests
test1 :: Principal
test1 = factorize $ Prim (N "p")

test2 :: Principal
test2 = factorize $ Prim (N "p") :∧ Prim (N "q")

test3 :: Principal
test3 = factorize $ Prim T :∧ Prim (N "p")

test4 :: Principal
test4 = factorize $ ((:→) $ Prim T) :∧ (Prim (N "p"))


        -- proof search --


-- Computing closure of principals under ←, →, ∨ and ∧
isatomic :: Principal -> Bool
isatomic p = case p of
  Prim n -> True
  (:←) p' -> isatomic p'
  (:→) p' -> isatomic p'
  _ -> False

confConfeq :: Principal -> Principal
confConfeq ((:→) (Prim B)) = Prim B
confConfeq p = p

optimizeConf :: S.Set Principal -> S.Set Principal
optimizeConf pl = S.fromList $ L.map (\x -> confConfeq x) (S.toList pl)


closureConf :: S.Set Principal -> S.Set Principal
closureConf ps = (optimizeConf ps')
  where ps' =  S.fromList [ (p :→) | p <- S.toList ps ]  


integIntegeq :: Principal -> Principal
integIntegeq ((:←) (Prim B)) = Prim B
integIntegeq p = p

optimizeInteg :: S.Set Principal -> S.Set Principal
optimizeInteg pl = S.fromList $ L.map (\x -> integIntegeq x) (S.toList pl)


closureInteg :: S.Set Principal -> S.Set Principal
closureInteg ps = (optimizeInteg ps')
  where ps' =  S.fromList [ (p :←) | p <- S.toList ps ]  

closureAtomic :: S.Set Principal -> S.Set Principal
closureAtomic  ps = S.union (S.fromList [Prim B]) $ S.union (closureConf ps)  (closureInteg ps)


closureConj :: S.Set Principal -> S.Set Principal
closureConj ps =  S.union ps $ S.fromList  [L.foldl1 (\a b -> a :∧ b)  p | p <- L.filter ((2<=).length) $ subsequences (S.toList ps)]

-- Given a set of principals, construct a set that is closed over :∨
closureDisj :: S.Set Principal -> S.Set Principal
closureDisj ps =  S.union ps $ S.fromList  [L.foldl1 (\a b -> a :∨ b)  p | p <- L.filter ((2<=).length) $ subsequences (S.toList ps)]
  
simplifyDisj :: S.Set Principal -> Principal -> (S.Set Principal, Principal)
simplifyDisj cache pl =
  case pl of
    (Prim T)  :∨ q -> simplifyDisj cache q
    (Prim B) :∨ q -> (cache, Prim B)
    (Prim (N s))  :∨ q ->
      if (S.member (Prim (N s)) cache)
      then  simplifyDisj (S.insert (Prim (N s)) cache) q
      else
        let (cache', q') = (simplifyDisj (S.insert (Prim (N s)) cache) q) in
          case q' of
                Prim B -> (cache', Prim B)
                Prim T -> (cache',  Prim (N s))
                _ -> (cache',  (Prim (N s)) :∨ q')
    (:←) (Prim (N s))  :∨ q ->
      if (S.member ((:←) (Prim (N s))) cache)
      then  simplifyDisj (S.insert ((:←) (Prim (N s))) cache) q
      else
        let (cache', q') = (simplifyDisj (S.insert ((:←) (Prim (N s))) cache) q) in 
          case q' of
                Prim B -> (cache', Prim B)
                Prim T -> (cache', ((:←) (Prim (N s))))
                _ -> (cache',  ((:←) (Prim (N s))) :∨ q')
    (:←) (Prim T)  :∨ q ->
      if (S.member ((:←) (Prim T)) cache)
      then  simplifyDisj (S.insert ((:←) (Prim T)) cache) q
      else
        let (cache', q') = (simplifyDisj (S.insert ((:←) (Prim T)) cache) q) in 
          case q' of
                Prim B -> (cache', Prim B)
                Prim T -> (cache',  ((:←) (Prim T)))
                _ -> (cache',  ((:←) (Prim T)) :∨ q')
    (:→) (Prim (N s))  :∨ q ->
      if (S.member ((:→) (Prim (N s))) cache)
      then  simplifyDisj (S.insert  ((:→) (Prim (N s))) cache) q 
      else
        let (cache', q') = (simplifyDisj (S.insert ((:→) (Prim (N s))) cache) q) in
          case q' of
                Prim B -> (cache', Prim B)
                Prim T -> (cache', ((:→) (Prim (N s))))
                _ -> (cache',  ((:→) (Prim (N s))) :∨ q')
    (:→) (Prim T)  :∨ q ->
      if (S.member ((:→) (Prim T)) cache)
      then  simplifyDisj (S.insert  ((:→) (Prim T)) cache) q 
      else
        let (cache', q') = (simplifyDisj (S.insert ((:→) (Prim T)) cache) q) in
          case q' of
                Prim B -> (cache', Prim B)
                Prim T -> (cache',  ((:→) (Prim T)))
                _ -> (cache',  ((:→) (Prim T)) :∨ q')
    p :∨ q ->
      let (cache', p') = simplifyDisj cache p in
        case p' of
          Prim T -> simplifyDisj cache' q
          Prim B -> (cache', Prim B)
          _ ->
            let (cache'', q') = simplifyDisj cache' q in
              case q' of
                Prim B -> (cache'', Prim B)
                Prim T -> (cache'', p')
                _ -> (cache'', p' :∨ q')
    _ -> (cache, pl)

      

-- Fixme: Given a set of principals closed under ∨, how to generate the set closed under ∧ ?
closureN :: S.Set Principal  -> S.Set Principal
closureN ps = let ps' = S.map (snd . simplifyDisj S.empty) $ closureDisj ps in
  closureConj ps'


closure :: S.Set L ->  S.Set Principal
closure ipset = let pl = S.map (\a -> case a of
                               T -> Prim T
                               B -> Prim B
                               N p -> Prim (N p)) ipset
                in
                  let platomic = closureAtomic pl in
--                    closureN  platomic
                    platomic


-- initial principal set for testing
initpset :: S.Set L
initpset = S.fromList [N "p", N "q", T, B]
--initpset = S.fromList [N "p", N "q"]



-- principals are already projected
search ::  DelContext -> Principal -> Principal -> Bool
search pi p q = -- search pi as per the robust inference rules
  -- static?
  if (staticR  p q) then True
  else if (assumeR pi p q) then True
  else if (conjR pi p q) then True
  else if (conjL pi p q) then True
  else if (disjR pi p q) then True
  else if (disjL pi p q) then True
  else let tclosure = transitive pi in
    if (transR tclosure p q) then True
    else False


pushConf :: Principal -> Principal
pushConf (p₁ :∨ p₂) = (pushConf p₁) :∨ (pushConf p₂)
pushConf  p = (:→) p

pushInteg :: Principal -> Principal
pushInteg (p₁ :∨ p₂) = (pushInteg p₁) :∨ (pushInteg p₂)
pushInteg  p = (:→) p


-- principals are not in normal form
proofsearch :: DelContext -> Principal -> Principal -> Bool
proofsearch del p q =
  let ((:→) pc) :∧ ((:←) pi) = factorize p in 
  let ((:→) qc) :∧  ((:←) qi)  = factorize q in
  let del' = S.map (\(p, q) -> let ((:→) pc) :∧ ((:←) pi) = factorize p in
                       let ((:→) qc) :∧ ((:←) qi) = factorize q in
                         ((pushConf pc) :∧ (pushInteg pi), (pushConf qc) :∧ (pushInteg qi))) del in  
    -- pc ∧ pi ≽ qc ∧ qi
    -- Search for pc ≽ qc and pi ≽ qi
    let bconf = search del' (pushConf pc) (pushConf qc)    in
      let binteg = search del' (pushInteg pi) (pushInteg qi)    in
        bconf && binteg

 -- Ł ⊢ p ≽ q
staticsearch ::  Principal -> Principal -> Bool
staticsearch p q =  case (p, q) of
  (_, _) | p == q -> True
  (_, Prim B) -> True             -- p ≽ ⊥ && p ≠ ⊥
  (_, Prim T ) -> False
  (Prim B, _) -> False
  (Prim T, _) -> True               -- ⊤ ≽ p && p ≠ ⊤
  ((:→) p', (:→) q') -> staticsearch p' q'    -- p ≽ q ⇒ (p->) ≽ (q->)
  ((:←) p', (:←) q') -> staticsearch p' q'    -- p ≽ q ⇒ (p<-) ≽ (q<-)
  ( _, (:←) p) -> True                        -- p ≽ (p->)
  (_, (:→) p) -> True                         -- p ≽ (p<-) 
  ((:←) q, _) -> True
  ((:→) q, _) -> True
  (p₁ :∧ p₂, _) -> (staticsearch p₁ q) || (staticsearch p₂ q)  -- p₁ ≽ q or p₂ ≽ q ⇒ p₁ ∧ p₂ ≽ q
  (_, q₁ :∧ q₂) -> (staticsearch p q₁) && (staticsearch p q₂)  -- p ≽ q₁ and p ≽ q₂ ⇒ p ≽ q₁ ∧ q₂
  (p₁ :∨ p₂, _) -> (staticsearch p₁ q) && (staticsearch p₂ q)  -- p₁ ≽ q and p₂ ≽ q ⇒ p₁ ∨ p₂ ≽ q
  (_, q₁ :∨ q₂) -> (staticsearch p q₁) || (staticsearch p q₂)  -- p ≽ q₁ or p ≽ q₂ ⇒ p ≽ q₁ ∨ q₂
  (_, _) -> False



-- Ł ⊢ p ≽ q
staticR :: Principal -> Principal -> Bool
staticR  p q = -- compute closure of N under →, ←, ∧ and ∨.
  staticsearch p q

-- p ≽ q ∈ Π ⇒ Π ⊢ p ≽ q 
assumeR :: DelContext -> Principal -> Principal -> Bool
assumeR pi p q = (S.member (p, q) pi)

-- Π ⊢ p ≽ q₁ and  Π ⊢ p ≽  q₂ ⇒ Π ⊢ p ≽ q₁ ∧ q₂ 
conjR :: DelContext -> Principal -> Principal -> Bool
conjR pi p (q₁ :∧ q₂) = (search pi p q₁) &&  (search pi p q₂)
conjR pi _ _ = False

-- Π ⊢ p₁ ≽ q or  Π ⊢ p₂ ≽  q ⇒ Π ⊢ p₁ ∧ p₂ ≽ q 
conjL :: DelContext -> Principal -> Principal -> Bool
conjL pi (p₁ :∧ p₂) q = (search pi p₁ q) ||  (search pi p₂ q)
conjL pi _ _ = False

-- Π ⊢ p₁ ≽ q and  Π ⊢ p₂ ≽ q  ⇒ Π ⊢ p₁ ∨ p₂ ≽ q 
disjL :: DelContext -> Principal -> Principal -> Bool
disjL pi (p₁ :∨ p₂)  q =  (search pi p₁ q) &&  (search pi p₂ q)
disjL pi _ _ = False

-- Π ⊢ p ≽ q₁ or  Π ⊢ p ≽ q₂  ⇒ Π ⊢ p ≽ q₁ ∨ q₂  
disjR :: DelContext -> Principal -> Principal -> Bool
disjR pi p (q₁ :∨ q₂) =  (search pi p q₁) ||  (search pi p q₂)
disjR pi _ _ = False

transR :: DelContext -> Principal -> Principal -> Bool
transR tclosure p q = S.member (p, q) tclosure
  
transitive :: DelContext -> DelContext
transitive pi
  | pi == pi' = pi
  | otherwise = transitive pi'
  where pi' = S.union pi (S.fromList [ (p,r) | (p, q) <- (S.toList pi),
                                       (q', r) <- (S.toList pi),
                                       q == q' ])
                           
-- test acts-of relation
-- p ≽ q, r ≽ q, q ≽ s
testpi = S.fromList [(Prim (N "p"), Prim (N "q")), (Prim (N "r"), Prim (N "q")),  (Prim (N "q"), Prim (N "s")) ]

-- p ≽ q, r ≽ q,  q ≽ s ⊩ ⊤ ≽ q   True
testproof1 = proofsearch testpi (Prim T) (Prim (N "q"))

-- p ≽ q, r ≽ q,  q ≽ s ⊩ ⊥ ≽ q   False
testproof2 = proofsearch testpi (Prim B) (Prim (N "q"))

-- p ≽ q, r ≽ q,  q ≽ s ⊩ p ∧ r ≽ q True
testproof3 = proofsearch testpi (Prim (N "p") :∧ Prim (N "r")) (Prim (N "q"))

-- p ≽ q, r ≽ q,  q ≽ s ⊩ p  ≽ s True
testproof4 = proofsearch testpi (Prim (N "p")) (Prim (N "s"))

-- p ≽ q, r ≽ q,  q ≽ s ⊩ r  ≽ s True 
testproof5 = proofsearch testpi (Prim (N "r")) (Prim (N "s"))

-- p ≽ q, r ≽ q,  q ≽ s ⊩ p  ≽ r  False
testproof6 = proofsearch testpi (Prim (N "p")) (Prim (N "r"))

             
  
