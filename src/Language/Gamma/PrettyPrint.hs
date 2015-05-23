{-# LANGUAGE OverloadedStrings #-}

module Language.Gamma.PrettyPrint () where

import Prelude hiding ((<$>))
import Control.Lens
import Text.PrettyPrint.ANSI.Leijen

import Language.Gamma.Types

commaSep :: Pretty a => [a] -> Doc
commaSep xs = commaSep' (fmap pretty xs)
commaSep' :: [Doc] -> Doc
commaSep' xs = sep (punctuate comma xs)

instance Pretty (GammaDecl a b) where
    pretty (VarDecl _ bind expr) = "let " <> pretty bind <> " = " <> pretty expr <> ";"
    pretty (FunDecl _ name args ret stmts) = "let " <> text name <> parens (commaSep args) <> retdoc ret <+> lbrace <$> indent 4 body <$> rbrace
        where retdoc Nothing = mempty
              retdoc (Just ty) = " : " <> pretty ty
              body = hcat (fmap pretty stmts)

instance Pretty (GammaBind a b) where
    pretty (PlainBind _ sym) = text sym
    pretty (TypeBind _ sym ty) = text sym <> " : " <> pretty ty

instance Pretty (GammaStmt a b) where
    pretty (DeclStmt _ decl) = pretty decl
    pretty (ExprStmt _ expr) = pretty expr <> ";"
    pretty (RetStmt _ expr) = "return " <> pretty expr <> ";"

instance Pretty (GammaType a) where
    pretty ty = go [] ty
        where go b (PrimType _ prim) = pretty prim
              go b (FunType _ ret args) = go b ret <> "^" <> parens (commaSep' (fmap (go b) args))
              go b (FreeType _ i) = char (freenames !! i)
              go b (UnivType _ t) = let c = boundnames !! length b in
                                    "forall " <> char c <> ". " <> go (c:b) t
              go b (BoundType _ i) = char (maybe '?' id (b ^? element i))
              go b (ConstrainedType _ bind args ty) = parens (text bind <+> hsep (fmap (go b) args)) <> " => " <> go b ty
              
              freenames = cycle ['n'..'z']
              boundnames = cycle ['a'..'m']

instance Pretty GammaPrimType where
    pretty CInt = "cint"

instance Pretty (GammaExpr a b) where
    pretty (LitExpr _ lit) = pretty lit
    pretty (SymExpr _ sym) = text sym
    pretty (TypeExpr _ expr ty) = pretty expr <> " : " <> pretty ty
    pretty (ApplyExpr _ name args) = pretty name <> parens (commaSep args)

instance Pretty GammaLit where
    pretty (IntLit i) = integer i
