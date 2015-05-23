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

ids = emptyIdents & styleReserved .~ HashSet.fromList ["let", "return", "cint"]
ops = emptyOps & styleReserved .~ HashSet.fromList ["=", ":"]

pSym = ident ids

pDecl :: GammaParser (GammaDecl () ())
pDecl =  try (VarDecl () <$> (reserve ids "let" *> pBind) <*> (reserve ops "=" *> pExpr <* symbol ";"))
     <|> FunDecl () <$> (reserve ids "let" *> pSym) <*> parens (commaSep pBind) <*> option Nothing (Just <$> (reserve ops ":" *> pType)) <*> braces (many pStmt)

pBind =  TypeBind () <$> try (pSym <* reserve ops ":") <*> pType
     <|> PlainBind () <$> pSym

pStmt =  DeclStmt () <$> pDecl
     <|> ExprStmt () <$> (pExpr <* symbol ";")
     <|> RetStmt () <$> (reserve ids "return" *> pExpr <* symbol ";")

pType = PrimType () <$> pPrimType
pPrimType = CInt <$ reserve ids "cint"

pExpr =  try (ApplyExpr () <$> pExprAtom <*> parens (commaSep pExpr))
     <|> try (ApplyExpr () <$> parens pExpr <*> parens (commaSep pExpr))
     <|> pExprAtom

pExprAtom =  LitExpr () <$> pLit
         <|> SymExpr () <$> pSym

pLit = IntLit <$> integer'
