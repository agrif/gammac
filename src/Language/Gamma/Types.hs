{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}

module Language.Gamma.Types where

import Control.Lens

class GammaAnnotated t where
    annotation :: Lens' (t a) a

type GammaSym = String

data GammaDecl t a = VarDecl a (GammaBind t a) (GammaExpr t a)
                   | FunDecl a GammaSym [GammaBind t a] (Maybe (GammaType t)) [GammaStmt t a]
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaDecl t) where
    annotation f (VarDecl a x y) = f a <&> \b -> VarDecl b x y
    annotation f (FunDecl a x y z w) = f a <&> \b -> FunDecl b x y z w

data GammaBind t a = PlainBind a GammaSym
                   | TypeBind a GammaSym (GammaType t)
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaBind t) where
    annotation f (PlainBind a x) = f a <&> \b -> PlainBind b x
    annotation f (TypeBind a x y) = f a <&> \b -> TypeBind b x y

data GammaStmt t a = DeclStmt a (GammaDecl t a)
                   | ExprStmt a (GammaExpr t a)
                   | RetStmt a (GammaExpr t a)
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaStmt t) where
    annotation f (DeclStmt a x) = f a <&> \b -> DeclStmt b x
    annotation f (ExprStmt a x) = f a <&> \b -> ExprStmt b x
    annotation f (RetStmt a x) = f a <&> \b -> RetStmt b x

data GammaType a = PrimType a GammaPrimType
                 | FunType a (GammaType a) [GammaType a]
                 | VarType a Int
                 | UnivType a (GammaType () -> GammaType a)
                 | BoundType a GammaSym [GammaType a] (GammaType a)
                 | DummyType String -- FIXME only here for show
                   deriving (Show, Functor)

instance GammaAnnotated GammaType where
    annotation f (PrimType a x) = f a <&> \b -> PrimType b x
    annotation f (FunType a x y) = f a <&> \b -> FunType b x y
    annotation f (VarType a x) = f a <&> \b -> VarType b x
    annotation f (UnivType a x) = f a <&> \b -> UnivType b x
    annotation f (BoundType a x y z) = f a <&> \b -> BoundType b x y z

instance (Show a) => Show (GammaType () -> GammaType a) where
    show f = show (f (DummyType "?"))

data GammaPrimType = CInt
                     deriving (Show, Eq)

data GammaExpr t a = LitExpr a GammaLit
                   | SymExpr a GammaSym
                   | TypeExpr a (GammaExpr t a) (GammaType t)
                   | ApplyExpr a (GammaExpr t a) [GammaExpr t a]
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaExpr t) where
    annotation f (LitExpr a x) = f a <&> \b -> LitExpr b x
    annotation f (SymExpr a x) = f a <&> \b -> SymExpr b x
    annotation f (TypeExpr a x y) = f a <&> \b -> TypeExpr b x y
    annotation f (ApplyExpr a x y) = f a <&> \b -> ApplyExpr b x y

data GammaLit = IntLit Integer
                deriving (Show)
