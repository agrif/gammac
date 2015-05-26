{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Gamma.Parser where

import Control.Applicative
import Control.Lens
import Data.Proxy
import qualified Data.HashSet as HashSet
import Text.Trifecta
import Text.Parser.Token.Style

import Language.Gamma.Types
import Language.Gamma.Parser.Commented

data GammaStyle
type GammaParser a = Commented GammaStyle Parser a

instance CommentedStyle (GammaStyle) where
    commentedStyle _ = javaCommentStyle & commentNesting .~ True

ids = emptyIdents & styleReserved .~ HashSet.fromList ["let", "return", "cint", "()"]
ops = emptyOps & styleReserved .~ HashSet.fromList ["=", ":"]

pSym = ident ids

pDecl :: GammaParser (GammaDecl () ())
pDecl = (reserve ids "let" *> decltype) <*> (reserve ops "=" *> pExpr <* symbol ";")
    where decltype =  try (FunDecl () <$> pSym <*> parens (commaSep pBind) <*> option Nothing (Just <$> (reserve ops ":" *> pType)))
                  <|> VarDecl () <$> pBind

pBind =  TypeBind () <$> try (pSym <* reserve ops ":") <*> pType
     <|> PlainBind () <$> pSym

pStmt =  DeclStmt () <$> pDecl
     <|> ExprStmt () <$> (pExpr <* symbol ";")

pType = PrimType () <$> pPrimType
pPrimType =  CInt <$ reserve ids "cint"
         <|> Unit <$ reserve ids "()"

pExpr =  try (ApplyExpr () <$> pExprAtom <*> parens (commaSep pExpr))
     <|> pExprAtom

pExprAtom =  LitExpr () <$> pLit
         <|> SymExpr () <$> pSym
         <|> parens pExpr
         <|> braces ( CompoundExpr () <$> many (try pStmt) <*> optionalRet)
    where optionalRet = maybe (LitExpr () UnitLit) id <$> optional pExpr

pLit =  IntLit <$> integer'
    <|> UnitLit <$ reserve ids "()"
