module SoilParser (parseString, parseFile) where

import SoilAst

import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isLetter)
import Control.Monad (liftM, (<=<))
import Control.Applicative ((<$>), (<*>))

type Error = String

parseString :: String -> Either Error Program
parseString s = case readP_to_S program s of
                  []    -> Left "Invalid program"
                  (x:_) -> Right $ fst x -- there shouldn't be multiple parses

parseFile :: FilePath -> IO (Either Error Program)
parseFile = return . parseString <=< readFile

program :: ReadP Program
program = do p <- defOps
             skipSpaces
             eof
             return p

defOps :: ReadP Program
defOps = do f <- funDef
            (fs, as) <- defOps
            return (f:fs, as)
     +++ do as <- many actOp
            return ([], as)

funDef :: ReadP Func
funDef = do stoken "let"
            i <- ident
            ps <- parens parameters
            stoken "from"
            n <- name
            ctoken '='
            e <- expr
            stoken "end"
            return $ Func i ps n e

expr :: ReadP Expr
expr = do stoken "if"
          p1 <- prim
          stoken "=="
          p2 <- prim
          stoken "then"
          e1 <- expr
          stoken "else"
          e2 <- expr
          stoken "end"
          return $ IfEq p1 p2 e1 e2
   +++ do stoken "case"
          p <- prim
          stoken "of"
          (c, e) <- cases
          stoken "end"
          return $ CaseOf p c e
   +++ liftM Acts (many actOp)

cases :: ReadP ([([Name], Expr)], Expr)
cases = do cs <- many case1
           d <- option (Acts []) cased
           return (cs, d)
    where case1 = do p <- parens parameters
                     ctoken ':'
                     e <- expr
                     return (p, e)
          cased = do ctoken '_'
                     ctoken ':'
                     expr

parameters :: ReadP [Name]
parameters = sepBy name (ctoken ',')

actOp :: ReadP ActOp
actOp = do stoken "become"
           (p, as) <- fcall
           return $ Become p as
    +++ do stoken "create"
           p1 <- prim
           stoken "with"
           (p2, as) <- fcall
           return $ Create p1 p2 as
    +++ do stoken "send"
           as <- parens args
           stoken "to"
           p <- prim
           return $ SendTo as p

fcall :: ReadP (Prim, [Prim])
fcall = do p <- prim
           as <- parens args
           return (p, as)

args :: ReadP [Prim]
args = sepBy prim (ctoken ',')

prim :: ReadP Prim
prim = chainl1 (choice [liftM Par name, liftM Id ident, self]) concatP
     where concatP = stoken "concat" >> return Concat
           self    = stoken "self"   >> return Self

ident :: ReadP Ident
ident = char '#' >> munch ((||) <$> isAlphaNum <*> (==) '_')

name :: ReadP Name
name = do c <- satisfy isLetter
          cs <- munch ((||) <$> isAlphaNum <*> (==) '_')
          let n = c:cs
          if n `elem` reserved then pfail
          else return n
    where reserved = ["let", "from", "case", "send", "self", "concat"
                     ,"become", "to", "create", "with", "case", "end"]

token :: ReadP a -> ReadP ()
token p = skipSpaces >> p >> skipSpaces

stoken :: String -> ReadP ()
stoken = token . string

ctoken :: Char -> ReadP ()
ctoken = token . char

parens :: ReadP a -> ReadP a
parens = between (ctoken '(') (ctoken ')')

