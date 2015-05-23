{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}

module Language.Gamma.Types where

type GammaSym = String

data GammaDecl t a = VarDecl a (GammaBind t a) (GammaExpr t a)
                   | FunDecl a GammaSym [GammaBind t a] (Maybe (GammaType t)) [GammaStmt t a]
                     deriving (Show, Functor, Foldable, Traversable)

data GammaBind t a = PlainBind a GammaSym
                   | TypeBind a GammaSym (GammaType t)
                     deriving (Show, Functor, Foldable, Traversable)

data GammaStmt t a = DeclStmt a (GammaDecl t a)
                   | ExprStmt a (GammaExpr t a)
                   | RetStmt a (GammaExpr t a)
                     deriving (Show, Functor, Foldable, Traversable)

data GammaType a = PrimType a GammaPrimType
                 | FunType a (GammaType a) [GammaType a]
                 | VarType a Int
                 | UnivType a (GammaType () -> GammaType a)
                 | BoundType a GammaSym [GammaType a] (GammaType a)
                 | DummyType String -- FIXME only here for show
                   deriving (Show, Functor)

gammaAnn (PrimType a _) = a
gammaAnn (FunType a _ _) = a
gammaAnn (VarType a _) = a
gammaAnn (UnivType a _) = a

instance (Show a) => Show (GammaType () -> GammaType a) where
    show f = show (f (DummyType "?"))

data GammaPrimType = CInt
                     deriving (Show, Eq)

data GammaExpr t a = LitExpr a GammaLit
                   | SymExpr a GammaSym
                   | TypeExpr a (GammaExpr t a) (GammaType t)
                   | ApplyExpr a (GammaExpr t a) [GammaExpr t a]
                     deriving (Show, Functor, Foldable, Traversable)

data GammaLit = IntLit Integer
                deriving (Show)
