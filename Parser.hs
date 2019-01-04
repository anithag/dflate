module Parser where

import qualified Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Data.List (elemIndex)
import Data.Map as M

import DflateTerm
--import CalderModule

data Info = Info { row :: Int, col :: Int } deriving (Show)

type CParser = Parsec String () 

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

     
binding :: CParser (String, Term)
binding =
  do name <- identifier
     symbol "="
     t <- term
     return (name, t)
  
gterm :: CParser Term
gterm =
  try (send <|> recv)
  <|> try (bind <|> protect)
  <|> actsfor
  <|> try (assume <|> tee)
  <|> try unit
  <|> try (first <|> second <|> pair)
  <|> (try injl <|> try injr <?> "boolean literal")
  <|> intLiteral
  <|> lam <|> parens term
  <|> try ite
  <|> (Var <$> identifier)

unaryPrim :: String -> (Term -> Term) -> CParser Term
unaryPrim s f =
  do symbol s
     t <- gterm
     return (f t)

first :: CParser Term
first = unaryPrim "fst" Fst

second :: CParser Term
second = unaryPrim "snd" Snd

pair :: CParser Term
pair = parens (do a <- term
                  P.comma lexer
                  b <- term
                  return (Pair a b)) <?> "a pair"


unit :: CParser Term
unit = symbol "()" >> return Unit


injl :: CParser Term
injl =  do reserved  "injl"
           e <- term
           reserved "as"
           ty <- sumty
           return (InjL e ty)


injr :: CParser Term
injr =  do reserved  "injr"
           e <- term
           reserved "as"
           ty <- sumty
           return (InjR e ty)

cond :: CParser Term
cond = injl <|> injr


ite :: CParser Term
ite = do reserved "case"
         c <- cond <?> "branch condition"
         reserved "inj1"
         x <- identifier
         e1 <- gterm
         reserved "inj2"
         y <- identifier
         e2 <- gterm
         return (Case c x e1 y e2)

intLiteral :: CParser Term
intLiteral = I <$> integer



send :: CParser Term
send = do reserved "send"
          ch <- identifier
          e1 <- term
          reserved "then"
          e2 <- term
          return (Send ch e1 e2)

recv :: CParser Term
recv = do reserved "receive"
          ch <- identifier
          reserved "as"
          x <- identifier
          e <- term
          return (Receive ch x e) 

term :: CParser Term
term =  (chainl1 gterm (return App))
       
dtype :: CParser DotType
dtype =
  try (unitty <|> intty)
  <|> actsty <|> sumty <|> prodty
  <|> funty
  <|> saysty <|>  dtype

ndtype :: CParser Type
ndtype =
  try (ddtype  <|> htype)


ddtype :: CParser Type
ddtype = do
  ty <- dtype
  return (Dot ty)
  
htype :: CParser Type
htype = do
  ty <- dtype
  symbol "#"
  return (Halt ty)
  
unitty :: CParser DotType
unitty = do
  symbol "()"
  return UnitTy

intty :: CParser DotType
intty = do
  symbol "int"
  return IntTy

actsty :: CParser DotType
actsty = do
  symbol ">"
  p1 <- principal
  p2 <- principal
  return (p1 :> p2)

sumty :: CParser DotType
sumty = do
  symbol "+"
  ty1 <- dtype
  ty2 <- dtype
  return (SumTy ty1 ty2)

prodty :: CParser DotType
prodty = do
  symbol "x"
  ty1 <- dtype
  ty2 <- dtype
  return (ProdTy ty1 ty2)

funty :: CParser DotType
funty = do
  symbol "->"
  ty1 <- dtype
  symbol "["
  pc <- principal
  symbol ","
  theta <- principal
  symbol "]"
  ty2 <- ndtype
  return (FunTy ty1 pc  M.empty ty2)

saysty :: CParser DotType
saysty = do
  l <- principal
  reserved "says"
  ty <- dtype
  return (SaysTy l ty)
  
lam :: CParser Term
lam = do symbol "\\"
         x <- identifier
         symbol ":"
         ty <- dtype
         symbol "["
         pc <- principal
         symbol ","
         theta <- principal 
         symbol "]"
         symbol "."
         t <- term
         return (Abs x ty pc M.empty t)

bind :: CParser Term
bind = do reserved "bind"
          x <- identifier
          symbol "=" 
          t1 <- term
          reserved "in"
          t2 <- term
          return (Bind x t1 t2)

dflatelabel :: CParser Label
dflatelabel  = do l <- identifier
                  return  (Prim (N l))
            
protect :: CParser Term
protect = do reserved "eta"
             l <- dflatelabel
             t <- term
             return (Protect l t)

principal :: CParser Principal
principal =
  confid <|> integrity
  <|>  conjunction <|> disjunction
  <|> parens principal
  <|> top <|> bottom
  <|> primitive
--  <|> computation
--  <|> (Prim  ltype)

   
primitive :: CParser Principal
primitive = do l <- identifier
               return (Prim (N l))

top, bottom :: CParser Principal
top = symbol "top" >> return (Prim T)

bottom = symbol "bot" >> return (Prim B)

confid, integrity :: CParser Principal
confid = do reserved "secret"
            p <- principal
            return ((:→) p)
            
integrity = do reserved "integrity"
               p <- principal
               return ((:←) p)

conjunction, disjunction :: CParser Principal
conjunction = do
  reserved "and"
  p1 <- principal
  p2 <- principal
  return (p1 :∧ p2)

disjunction = do
  reserved "or"
  p1 <- principal
  p2 <- principal
  return (p1 :∨ p2)

{-
computation :: CParser Principal
computation = do
  l <- identifier
  return (T l)
 -}
  
tee :: CParser Term
tee = do reserved "tee"
         t <- principal
         e <- term
         return (TEE t e)

actsfor :: CParser Term
actsfor = do
  symbol ">"
  p1 <- principal
  p2 <- principal
  return (Actsfor p1 p2)
  
assume :: CParser Term
assume = do reserved "assume"
            t1 <- term
            reserved "in"
            t2 <- term
            return (Assume t1 t2)

parserprincipal :: CParser Principal
parserprincipal = do whiteSpace
                     t <- principal
                     eof
                     return t

parser :: CParser Term
parser = do whiteSpace
            t <- term
            eof
            return t
{-
 Using a Haskell lexer as a base
-}
lexer = P.makeTokenParser haskellDef
parens = P.parens lexer
braces = P.braces lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
integer = P.integer lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
