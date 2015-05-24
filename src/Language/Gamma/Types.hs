{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, TemplateHaskell #-}

module Language.Gamma.Types where

import Control.Lens
import Control.Lens.TH

class GammaAnnotated t where
    annotation :: Lens' (t a) a

type GammaSym = String

class (HasGammaSym a) where
    gammaSym :: Lens' a GammaSym

instance (HasGammaSym GammaSym) where
    gammaSym = id

data GammaDecl t a = VarDecl a (GammaBind t a) (GammaExpr t a)
                   | FunDecl a GammaSym [GammaBind t a] (Maybe (GammaType t)) [GammaStmt t a]
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaDecl t) where
    annotation f (VarDecl a x y) = f a <&> \b -> VarDecl b x y
    annotation f (FunDecl a x y z w) = f a <&> \b -> FunDecl b x y z w

instance HasGammaSym (GammaDecl t a) where
    gammaSym f (VarDecl a x y) = gammaSym f x <&> \x' -> VarDecl a x' y
    gammaSym f (FunDecl a x y z w) = gammaSym f x <&> \x' -> FunDecl a x' y z w

data GammaBind t a = PlainBind a GammaSym
                   | TypeBind a GammaSym (GammaType t)
                     deriving (Show, Functor, Foldable, Traversable)

instance GammaAnnotated (GammaBind t) where
    annotation f (PlainBind a x) = f a <&> \b -> PlainBind b x
    annotation f (TypeBind a x y) = f a <&> \b -> TypeBind b x y

instance HasGammaSym (GammaBind t a) where
    gammaSym f (PlainBind a x) = gammaSym f x <&> \x' -> PlainBind a x'
    gammaSym f (TypeBind a x y) = gammaSym f x <&> \x' -> TypeBind a x' y

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
                 | FreeType a Int
                 | UnivType a (GammaType a) -- de bruijen indexing
                 | BoundType a Int
                 | ConstrainedType a GammaSym [GammaType a] (GammaType a)
                   deriving (Show, Functor)

instance GammaAnnotated GammaType where
    annotation f (PrimType a x) = f a <&> \b -> PrimType b x
    annotation f (FunType a x y) = f a <&> \b -> FunType b x y
    annotation f (FreeType a x) = f a <&> \b -> FreeType b x
    annotation f (UnivType a x) = f a <&> \b -> UnivType b x
    annotation f (BoundType a x) = f a <&> \b -> BoundType b x
    annotation f (ConstrainedType a x y z) = f a <&> \b -> ConstrainedType b x y z

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

makePrisms ''GammaDecl
makePrisms ''GammaBind
makePrisms ''GammaStmt
makePrisms ''GammaType
makePrisms ''GammaPrimType
makePrisms ''GammaExpr
makePrisms ''GammaLit
