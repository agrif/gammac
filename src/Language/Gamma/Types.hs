{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

module Language.Gamma.Types where

import Control.Lens
import Control.Lens.TH
import Data.Data
import Data.Data.Lens

type GammaSym = String

data GammaDecl t a = VarDecl a (GammaBind t a) (GammaExpr t a)
                   | FunDecl a GammaSym [GammaBind t a] (Maybe (GammaType t)) (GammaExpr t a)
                     deriving (Show, Functor, Foldable, Traversable, Data)

data GammaBind t a = PlainBind a GammaSym
                   | TypeBind a GammaSym (GammaType t)
                     deriving (Show, Functor, Foldable, Traversable, Data)

data GammaStmt t a = DeclStmt a (GammaDecl t a)
                   | ExprStmt a (GammaExpr t a)
                     deriving (Show, Functor, Foldable, Traversable, Data)

data GammaType a = PrimType a GammaPrimType
                 | FunType a (GammaType a) [GammaType a]
                 | FreeType a Int
                 | UnivType a (GammaType a) -- de bruijen indexing
                 | BoundType a Int
                 | ConstrainedType a GammaSym [GammaType a] (GammaType a)
                   deriving (Show, Functor, Data)

data GammaPrimType = CInt
                   | Unit
                     deriving (Show, Eq, Data)

data GammaExpr t a = LitExpr a GammaLit
                   | SymExpr a GammaSym
                   | TypeExpr a (GammaExpr t a) (GammaType t)
                   | ApplyExpr a (GammaExpr t a) [GammaExpr t a]
                   | CompoundExpr a [GammaStmt t a] (GammaExpr t a)
                     deriving (Show, Functor, Foldable, Traversable, Data)

data GammaLit = IntLit Integer
              | UnitLit
                deriving (Show, Data)

makePrisms ''GammaDecl
makePrisms ''GammaBind
makePrisms ''GammaStmt
makePrisms ''GammaType
makePrisms ''GammaPrimType
makePrisms ''GammaExpr
makePrisms ''GammaLit

class GammaAnnotated t a | t -> a where
    annotation :: Lens' t a
    default annotation :: (Data t, Typeable a) => Lens' t a
    annotation = singular template

instance (Data t, Data a) => GammaAnnotated (GammaDecl t a) a
instance (Data t, Data a) => GammaAnnotated (GammaBind t a) a
instance (Data t, Data a) => GammaAnnotated (GammaStmt t a) a
instance (Data a) => GammaAnnotated (GammaType a) a
instance (Data t, Data a) => GammaAnnotated (GammaExpr t a) a

class (HasGammaSym a) where
    gammaSym :: Lens' a GammaSym

instance (HasGammaSym GammaSym) where
    gammaSym = id

instance HasGammaSym (GammaDecl t a) where
    gammaSym f (VarDecl a x y) = gammaSym f x <&> \x' -> VarDecl a x' y
    gammaSym f (FunDecl a x y z w) = gammaSym f x <&> \x' -> FunDecl a x' y z w

instance HasGammaSym (GammaBind t a) where
    gammaSym f (PlainBind a x) = gammaSym f x <&> \x' -> PlainBind a x'
    gammaSym f (TypeBind a x y) = gammaSym f x <&> \x' -> TypeBind a x' y

