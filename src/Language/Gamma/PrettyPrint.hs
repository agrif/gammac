{-# LANGUAGE OverloadedStrings #-}

module Language.Gamma.PrettyPrint () where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Language.Gamma.Types

commaSep :: Pretty a => [a] -> Doc
commaSep xs = commaSep' (fmap pPrint xs)
commaSep' :: [Doc] -> Doc
commaSep' xs = sep (punctuate comma xs)

instance Pretty (GammaDecl a) where
    pPrint (VarDecl _ bind expr) = "let " <> pPrint bind <> " = " <> pPrint expr <> ";"
    pPrint (FunDecl _ name args ret stmts) = "let " <> text name <> parens (commaSep args) <> retdoc ret <+> lbrace $+$ nest 4 body $+$ rbrace
        where retdoc Nothing = mempty
              retdoc (Just ty) = " : " <> pPrint ty
              body = hcat (fmap pPrint stmts)

instance Pretty (GammaBind a) where
    pPrint (PlainBind _ sym) = text sym
    pPrint (TypeBind _ sym ty) = text sym <> " : " <> pPrint ty

instance Pretty (GammaStmt a) where
    pPrint (DeclStmt _ decl) = pPrint decl
    pPrint (ExprStmt _ expr) = pPrint expr <> ";"
    pPrint (RetStmt _ expr) = "return " <> pPrint expr <> ";"

instance Pretty (GammaType a) where
    pPrint ty = go 0 ty
        where go n (PrimType _ prim) = pPrint prim
              go n (FunType _ ret args) = go n ret <> "^" <> parens (commaSep' (fmap (go n) args))
              go n (VarType _ i) = char (varnames !! i)
              go n (UnivType _ f) = "forall " <> char (dummynames !! n) <> ". " <> go (n + 1) (f (DummyType (dummynames !! n : [])))
              go n (BoundType _ bind args ty) = parens (text bind <+> hsep (fmap (go n) args)) <> " => " <> go n ty
              go n (DummyType s) = text s
              
              varnames = cycle ['n'..'z']
              dummynames = cycle ['a'..'m']
    -- quantification

instance Pretty GammaPrimType where
    pPrint CInt = "cint"

instance Pretty (GammaExpr a) where
    pPrint (LitExpr _ lit) = pPrint lit
    pPrint (SymExpr _ sym) = text sym
    pPrint (TypeExpr _ expr ty) = pPrint expr <> " : " <> pPrint ty
    pPrint (ApplyExpr _ name args) = pPrint name <> parens (commaSep args)

instance Pretty GammaLit where
    pPrint (IntLit i) = integer i
