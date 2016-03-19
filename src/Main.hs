{-
 v001=Trim(0,999)
 v002=Trim(1000,1049)
 timecode="timecode.txt"
-}
{-
expr ::= var | funcall | ( expr ) | expr . expr
var  ::= letter {  letter | digit }*
funcall ::= var ( { expr { , expr }* }? )
stmt ::= var = expr | stmt { "\n" stmt }+
-}
module Main where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Fun String Expr | Dot Expr Expr
            deriving Show

def :: LanguageDef ()
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
           , whiteSpace = m_whiteSpace } = makeTokenParser def

main :: IO ()
main = do
  print $ Fun "AAA" (Var "BBB")