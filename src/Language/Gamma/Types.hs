{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Language.Gamma.Types where

type GammaSym = String

data GammaDecl a = VarDecl a (GammaBind a) (GammaExpr a)
                 | FunDecl a GammaSym [GammaBind a] (Maybe (GammaType a)) [GammaStmt a]
                   deriving (Show, Functor)

data GammaBind a = PlainBind a GammaSym
                 | TypeBind a GammaSym (GammaType a)
                   deriving (Show, Functor)

data GammaStmt a = DeclStmt a (GammaDecl a)
                 | ExprStmt a (GammaExpr a)
                 | RetStmt a (GammaExpr a)
                   deriving (Show, Functor)

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

data GammaExpr a = LitExpr a GammaLit
                 | SymExpr a GammaSym
                 | TypeExpr a (GammaExpr a) (GammaType a)
                 | ApplyExpr a (GammaExpr a) [GammaExpr a]
                   deriving (Show, Functor)

data GammaLit = IntLit Integer
                deriving (Show)
