{-
 v001=Trim(0,999)
 v002=Trim(1000,1049)
 timecode="timecode.txt"
-}
{-
expr ::= var | funcall | '(' expr ') | expr '.' expr
var  ::= letter { letter | digit }*
funcall ::= var ( { expr { ',' expr }* }? )
stmt ::= var = expr | stmt { "\n" stmt }+
-}
module Main where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Fun String [Expr] | Dot Expr Expr
            deriving Show
data Stmt = Bare Expr | Assign String Expr | Seq [Stmt]
            deriving Show

def :: LanguageDef st
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=."
              , opLetter = oneOf "=."
              , reservedOpNames = ["=", "."]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace
           , integer = m_integer
           , commaSep = m_commaSep } = makeTokenParser def

term :: Parser Expr
term = m_parens expr
       <|> funCall
       <|> fmap Var m_identifier

funCall :: Parser Expr
funCall = try $ do
  name <- m_identifier
  e <- m_parens $ m_commaSep expr
  return $ Fun name e

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"
  where table = [
                  [Infix (m_whiteSpace >> m_reservedOp "." >> m_whiteSpace >> return Dot) AssocLeft]
                ]

stmt :: Parser Stmt
stmt = do
  m_whiteSpace
  e <- expr
  return $ Seq [Bare e]
  <* eof

main :: IO ()
main = do
  inp <- getLine
  print $ parse stmt "" inp
