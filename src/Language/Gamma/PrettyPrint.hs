{-# LANGUAGE OverloadedStrings #-}

module Language.Gamma.PrettyPrint () where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

import Language.Gamma.Types

commaSep :: Pretty a => [a] -> Doc
commaSep xs = commaSep' (fmap pretty xs)
commaSep' :: [Doc] -> Doc
commaSep' xs = sep (punctuate comma xs)

instance Pretty (GammaDecl a) where
    pretty (VarDecl _ bind expr) = "let " <> pretty bind <> " = " <> pretty expr <> ";"
    pretty (FunDecl _ name args ret stmts) = "let " <> text name <> parens (commaSep args) <> retdoc ret <+> lbrace <$> indent 4 body <$> rbrace
        where retdoc Nothing = mempty
              retdoc (Just ty) = " : " <> pretty ty
              body = hcat (fmap pretty stmts)

instance Pretty (GammaBind a) where
    pretty (PlainBind _ sym) = text sym
    pretty (TypeBind _ sym ty) = text sym <> " : " <> pretty ty

instance Pretty (GammaStmt a) where
    pretty (DeclStmt _ decl) = pretty decl
    pretty (ExprStmt _ expr) = pretty expr <> ";"
    pretty (RetStmt _ expr) = "return " <> pretty expr <> ";"

instance Pretty (GammaType a) where
    pretty ty = go 0 ty
        where go n (PrimType _ prim) = pretty prim
              go n (FunType _ ret args) = go n ret <> "^" <> parens (commaSep' (fmap (go n) args))
              go n (VarType _ i) = char (varnames !! i)
              go n (UnivType _ f) = "forall " <> char (dummynames !! n) <> ". " <> go (n + 1) (f (DummyType (dummynames !! n : [])))
              go n (BoundType _ bind args ty) = parens (text bind <+> hsep (fmap (go n) args)) <> " => " <> go n ty
              go n (DummyType s) = text s
              
              varnames = cycle ['n'..'z']
              dummynames = cycle ['a'..'m']
    -- quantification

instance Pretty GammaPrimType where
    pretty CInt = "cint"

instance Pretty (GammaExpr a) where
    pretty (LitExpr _ lit) = pretty lit
    pretty (SymExpr _ sym) = text sym
    pretty (TypeExpr _ expr ty) = pretty expr <> " : " <> pretty ty
    pretty (ApplyExpr _ name args) = pretty name <> parens (commaSep args)

instance Pretty GammaLit where
    pretty (IntLit i) = integer i
