{-# LANGUAGE OverloadedStrings #-}

module Language.Gamma.Parser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Text hiding (takeWhile)

import Language.Gamma.Types

pSym = unpack <$> identifier

pDecl =  VarDecl () <$> (reserved "let" *> pBind) <*> (token "=" *> pExpr <* token ";")
     <|> FunDecl () <$> (reserved "let" *> pSym) <*> parens (commaSep pBind) <*> option Nothing (Just <$> (token ":" *> pType)) <*> braces (many pStmt)

pBind =  TypeBind () <$> pSym <* token ":" <*> pType
     <|> PlainBind () <$> pSym

pStmt =  DeclStmt () <$> pDecl
     <|> ExprStmt () <$> (pExpr <* token ";")
     <|> RetStmt () <$> (reserved "return" *> pExpr <* token ";")

-- FIXME VarType, UnivType
pType = PrimType () <$> pPrimType

pPrimType = CInt <$ reserved "cint"

-- FIXME TypeExpr
pExpr =  ApplyExpr () <$> pExprAtom <*> parens (commaSep pExpr)
     <|> ApplyExpr () <$> parens pExpr <*> parens (commaSep pExpr)
     <|> pExprAtom

pExprAtom =  LitExpr () <$> pLit
         <|> SymExpr () <$> pSym

pLit = IntLit <$> signed ("0x" *> hexadecimal <|> decimal)

identifier = lexeme (fst <$> match (first <* rest) <?> "identifier")
    where first = satisfy (inClass "a-zA-Z_")
          rest = takeWhile (inClass "a-zA-Z_0-9")
reserved name = (identifier >>= \s -> guard (s == pack name) >> return s) <?> name

commaSep x = x `sepBy` token ","
braces x = token "{" *> x <* token "}"
parens x = token "(" *> x <* token ")"
token x = lexeme (string x) <?> unpack x
lexeme x = x <* whitespace
whitespace = many (  void (takeWhile1 isHorizontalSpace)
                 <|> void singleLineComment
                 <|> void multiLineComment
                 <|> endOfLine
                  ) <?> "whitespace"
    where singleLineComment = "//" *> takeTill isEndOfLine <* endOfLine
          multiLineComment = "/*" *> many inside *> "*/"
          
          inside =  takeWhile1 (notInClass "/*")
                <|> multiLineComment
                <|> ("*" <* notChar '/')
