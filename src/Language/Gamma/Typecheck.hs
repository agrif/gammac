{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TupleSections, TemplateHaskell #-}

module Language.Gamma.Typecheck where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Lens
import Control.Lens.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Gamma.Types
import Language.Gamma.Builtins

declareLenses [d|
  data TypState ann = TypState
      { nextTypeVar :: Int
      , bindings :: Map GammaSym (GammaType ann)
      , substitutions :: Map Int (GammaType ann)
      , constraints :: [(ann, GammaSym, [GammaType ann])]
      } deriving (Show)
  |]

data TypError a = UnboundSymbol a GammaSym
                | InfiniteType (GammaType a) (GammaType a)
                | ArgumentCount (GammaType a) (GammaType a)
                | CannotUnify (GammaType a) (GammaType a)
                | DoesNotReturn a GammaSym
                | BrokenCompiler
                  deriving (Show)

type TypT ann m a = StateT (TypState ann) (ExceptT (TypError ann) m) a
type Typ ann a = TypT ann Identity a

builtinBindings :: Map GammaSym (GammaType ())
builtinBindings = Map.fromList [(builtinName b, builtinType b) | b <- builtins]

runTypT :: (Functor m) => Map GammaSym (GammaType ann) -> TypT ann m a -> m (Either (TypError ann) a)
runTypT table m = runExceptT (fst <$> runStateT m state)
    where state = TypState 0 table mempty []

runTyp :: Map GammaSym (GammaType ann) -> Typ ann a -> Either (TypError ann) a
runTyp table = runIdentity . runTypT table

createVar' :: (Monad m) => TypT ann m Int
createVar' = use nextTypeVar <* (nextTypeVar += 1)

createVar :: (Monad m) => a -> TypT ann m (GammaType a)
createVar a = FreeType a <$> createVar'

scoped :: (Monad m) => TypT ann m a -> TypT ann m a
scoped m = do
  binds <- use bindings
  r <- m
  bindings .= binds
  return r

lookupSym :: (Monad m) => a -> GammaSym -> TypT a m (GammaType a)
lookupSym a name = do
  entry <- uses bindings (Map.lookup name)
  case entry of
    Just ty -> return ty
    Nothing -> throwError (UnboundSymbol a name)

bindSym :: (Monad m) => GammaSym -> GammaType a -> TypT a m (GammaType a)
bindSym name ty = ty <$ (bindings %= Map.insert name ty)

class GammaTypes t a | t -> a where
    types :: Traversal' t (GammaType a)

mapType :: (GammaTypes t a) => Int -> GammaType a -> t -> t
mapType n ty orig = orig & types %~ replace
    where replace t@(FreeType a m) | m == n    = ty
                                   | otherwise = t
          replace t = t

freeTypeVars :: (GammaTypes t a) => t -> Set Int
freeTypeVars ty = ty ^. types._FreeType._2.to Set.singleton

instance GammaTypes (GammaType a) a where
    types f (FunType a fun args) = FunType a <$> types f fun <*> traverse (types f) args
    types f (UnivType a t) = UnivType a <$> types f t
    types f (ConstrainedType a name args t) = ConstrainedType a name <$> traverse (types f) args <*> types f t
    types f t = f t

instance GammaTypes (Map k (GammaType a)) a where
    types = traverse.types

instance GammaTypes [(a, GammaSym, [GammaType a])] a where
    types = traverse._3.traverse.types

instance GammaTypes (TypState a) a where
    types f (TypState x bind subst const) = TypState x <$> types f bind <*> types f subst <*> types f const

elimTyp :: (Monad m) => Int -> GammaType a -> TypT a m (GammaType a)
elimTyp n ty = do
  modify (mapType n ty)
  substitutions %= Map.insert n ty
  return ty

constrainTyp :: (Monad m) => a -> GammaSym -> [GammaType a] -> TypT a m ()
constrainTyp a name tys = constraints %= ((a, name, tys) :)

class (Traversable (t a)) => GammaTypeable t a where
    typecheck :: (Monad m) => t a (a, Int) -> TypT a m (GammaType a)

instantiate :: GammaType a -> GammaType a -> GammaType a
instantiate templ v = go 0 templ
    where go n (FunType a x ys) = FunType a (go n x) (fmap (go n) ys)
          go n (UnivType a x) = UnivType a (go (n + 1) x)
          go n (ConstrainedType a b xs y) = ConstrainedType a b (fmap (go n) xs) (go n y)
          go n t@(BoundType a m) | n == m    = v
                                 | otherwise = t
          go n t = t

abstract :: GammaType a -> Int -> GammaType a
abstract templ n | Set.member n (freeTypeVars templ) = UnivType (templ ^. annotation) (go 0 templ)
                 | otherwise = templ
   where go i (FunType a x ys) = FunType a (go i x) (fmap (go i) ys)
         go i (UnivType a x) = UnivType a (go (i + 1) x)
         go i (ConstrainedType a b xs y) = ConstrainedType a b (fmap (go i) xs) (go i y)
         go i t@(FreeType a m) | n == m    = BoundType a i
                               | otherwise = t
         go i t = t

unify :: (Monad m) => GammaType a -> GammaType a -> TypT a m (GammaType a)
unify ty@(FreeType a n) (FreeType b m)
    | n == m    = return ty
    | otherwise = elimTyp m ty
unify ty1@(FreeType a n) ty2
    | Set.member n (freeTypeVars ty2) = throwError (InfiniteType ty1 ty2)
    | otherwise = elimTyp n ty2
unify ty1 ty2@(FreeType a n) = unify ty2 ty1

unify ty1@(UnivType a t) ty2 = createVar a >>= unify ty2 . instantiate t
unify ty1 ty2@(UnivType a t) = unify ty2 ty1

unify ty1@(ConstrainedType a name args t) ty2 = do
  constrainTyp a name args
  unify t ty2
unify ty1 ty2@(ConstrainedType a name args t) = unify ty2 ty1

unify ty1@(FunType a f1 a1) ty2@(FunType b f2 a2)
    | length a1 /= length a2 = throwError (ArgumentCount ty1 ty2)
    | otherwise =
        do f <- unify f1 f2
           args <- mapM (uncurry unify) (zip a1 a2)
           return (FunType a f args)
unify ty1@(PrimType _ p1) ty2@(PrimType _ p2) | p1 == p2 = return ty1
                                              | otherwise = throwError (CannotUnify ty1 ty2)
unify ty1 ty2 = throwError (CannotUnify ty1 ty2)

generalize :: (Monad m) => TypT a m (GammaType a) -> TypT a m (GammaType a)
generalize m = do
  oldc <- use constraints
  constraints .= []
  
  ty <- m
  cs <- use constraints
  env <- uses bindings freeTypeVars
  let abs = Set.difference (freeTypeVars ty) env
  
  constraints .= oldc
  itraverse (\i n -> elimTyp n (BoundType (ty ^. annotation) i)) (Set.toList abs)
  return (foldl abstract (foldl constrain ty cs) abs)

    where constrain ty (a, name, args) = ConstrainedType a name args ty

instance GammaTypeable GammaDecl a where
    typecheck (VarDecl (a, i) bind expr) =
        do ety <- typecheck expr
           bty <- typecheck bind
           elimTyp i =<< (generalize $ unify bty ety)
    typecheck (FunDecl (a, i) name binds ret stmts) =
        do fty <- generalize $ scoped $ do
                    args <- mapM typecheck binds
                    rty <- maybe (createVar a) return ret
                    bindSym name (FunType a rty args)
                    forM_ stmts $ \stmt -> do
                        ty <- typecheck stmt
                        case stmt of
                          (RetStmt _ _) -> unify rty ty
                          _ -> return ty
                    lookupSym a name
           
           elimTyp i =<< bindSym name fty

instance GammaTypeable GammaBind a where
    typecheck (PlainBind (a, i) sym) = createVar a >>= bindSym sym >>= elimTyp i
    typecheck (TypeBind (a, i) sym ty) = bindSym sym ty >>= elimTyp i

instance GammaTypeable GammaStmt a where
    typecheck (DeclStmt (a, i) decl) = typecheck decl >>= elimTyp i
    typecheck (ExprStmt (a, i) expr) = typecheck expr >>= elimTyp i
    typecheck (RetStmt (a, i) expr) = typecheck expr >>= elimTyp i

instance GammaTypeable GammaExpr a where
    typecheck (LitExpr (a, i) (IntLit _)) = elimTyp i (PrimType a CInt)
    typecheck (SymExpr (a, i) sym) = lookupSym a sym >>= elimTyp i
    typecheck (TypeExpr (a, i) expr ty) =
        do ity <- typecheck expr
           elimTyp i =<< unify ity ty
    typecheck (ApplyExpr (a, i) fun args) =
        do ret <- createVar a
           fty <- typecheck fun
           argty <- mapM typecheck args
           FunType _ rty _ <- unify fty (FunType a ret argty)
           elimTyp i rty

inferTypes :: (Monad m, GammaTypeable t a) => t a a -> TypT a m (t a (a, GammaType a))
inferTypes code = do
    annotated <- (traverse (\a -> (a,) <$> createVar') code)
    typecheck annotated
    subst <- use substitutions
    let final = traverse (\(a, i) -> (a,) <$> Map.lookup i subst) annotated
    case final of
      Just f -> return f
      Nothing -> throwError BrokenCompiler
