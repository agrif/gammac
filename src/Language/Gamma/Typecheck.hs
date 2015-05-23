{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Language.Gamma.Typecheck where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Gamma.Types
import Language.Gamma.Builtins

data TypState ann = TypState
    { nextTypeVar :: Int
    , bindings :: Map GammaSym (GammaType ann)
    , constraints :: [(ann, GammaSym, [GammaType ann])]
    } deriving (Show)

data TypError a = UnboundSymbol a GammaSym
                | InfiniteType (GammaType a) (GammaType a)
                | ArgumentCount (GammaType a) (GammaType a)
                | CannotUnify (GammaType a) (GammaType a)
                | DoesNotReturn a GammaSym
                  deriving (Show)

type TypT ann m a = StateT (TypState ann) (ExceptT (TypError ann) m) a
type Typ ann a = TypT ann Identity a

builtinBindings :: Map GammaSym (GammaType ())
builtinBindings = Map.fromList [(builtinName b, builtinType b) | b <- builtins]

runTypT :: (Functor m) => Map GammaSym (GammaType ann) -> TypT ann m a -> m (Either (TypError ann) a)
runTypT table m = runExceptT (fst <$> runStateT m state)
    where state = TypState
                  { nextTypeVar = 0
                  , bindings = table
                  , constraints = []
                  }

runTyp :: Map GammaSym (GammaType ann) -> Typ ann a -> Either (TypError ann) a
runTyp table = runIdentity . runTypT table

createVar :: (Monad m) => a -> TypT ann m (GammaType a)
createVar a = do
  n <- gets nextTypeVar
  modify (\s -> s { nextTypeVar = n + 1 })
  return (VarType a n)

scoped :: (Monad m) => TypT ann m a -> TypT ann m a
scoped m = do
  binds <- gets bindings
  r <- m
  modify (\s -> s { bindings = binds })
  return r

lookupSym :: (Monad m) => a -> GammaSym -> TypT a m (GammaType a)
lookupSym a name = do
  entry <- gets (Map.lookup name . bindings)
  case entry of
    Just ty -> return ty
    Nothing -> throwError (UnboundSymbol a name)

bindSym :: (Monad m) => GammaSym -> GammaType a -> TypT a m (GammaType a)
bindSym name ty = ty <$ modify (\s -> s { bindings = Map.insert name ty (bindings s) })

class GammaTypes t a | t -> a where
    mapType :: Int -> GammaType a -> t -> t
    freeTypeVars :: t -> Set Int

instance GammaTypes (GammaType a) a where
    mapType n ty (FunType a fun args) = FunType a (mapType n ty fun) (fmap (mapType n ty) args)
    mapType n ty orig@(VarType a m) | n == m    = ty
                                    | otherwise = orig
    mapType n ty (UnivType a f) = UnivType a (fmap (mapType n ty) f)
    mapType n ty (BoundType a name args t) = BoundType a name (fmap (mapType n ty) args) (mapType n ty t)
    mapType _ _ ty = ty
    
    freeTypeVars (FunType a fun args) = foldl Set.union (freeTypeVars fun) (fmap freeTypeVars args)
    freeTypeVars (VarType a n) = Set.singleton n
    freeTypeVars (UnivType a f) = freeTypeVars (f (DummyType ""))
    freeTypeVars (BoundType a _ args ty) = foldl Set.union (freeTypeVars ty) (fmap freeTypeVars args)
    freeTypeVars _ = mempty

instance GammaTypes (Map GammaSym (GammaType a)) a where
    mapType n ty table = fmap (mapType n ty) table
    freeTypeVars table = foldl Set.union mempty (fmap freeTypeVars table)

instance GammaTypes (TypState a) a where
    mapType n ty state = state
                         { bindings = mapType n ty (bindings state)
                         , constraints = [(a, name, [mapType n ty t | t <- tys]) | (a, name, tys) <- constraints state]
                         }
    freeTypeVars state = foldl Set.union (freeTypeVars (bindings state)) [freeTypeVars t | (_, _, tys) <- constraints state, t <- tys]

elimTyp :: (Monad m) => Int -> GammaType a -> TypT a m (GammaType a)
elimTyp n ty = do
  modify (mapType n ty)
  return ty

constrainTyp :: (Monad m) => a -> GammaSym -> [GammaType a] -> TypT a m ()
constrainTyp a name tys = modify (\s -> s { constraints = (a, name, tys) : constraints s })

class GammaTypeable t a where
    typecheck :: (Monad m) => t a -> TypT a m (GammaType a)

unify :: (Monad m) => GammaType a -> GammaType a -> TypT a m (GammaType a)
unify ty@(VarType a n) (VarType b m)
    | n == m    = return ty
    | otherwise = elimTyp m ty
unify ty1@(VarType a n) ty2
    | Set.member n (freeTypeVars ty2) = throwError (InfiniteType ty1 ty2)
    | otherwise = elimTyp n ty2
unify ty1 ty2@(VarType a n) = unify ty2 ty1

unify ty1@(UnivType a f) ty2 = createVar () >>= unify ty2 . f
unify ty1 ty2@(UnivType a f) = unify ty2 ty1

unify ty1@(BoundType a name args t) ty2 = do
  constrainTyp a name args
  unify t ty2
unify ty1 ty2@(BoundType a name args t) = unify ty2 ty1

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
  oldc <- gets constraints
  modify (\s -> s { constraints = [] })
  
  ty <- m
  cs <- gets constraints
  env <- gets freeTypeVars
  let abs = Set.difference (freeTypeVars ty) env
  
  modify (\s -> s { constraints = oldc })
  return (foldl abstract (foldl constrain ty cs) abs)

    where abstract t n
              | Set.member n (freeTypeVars t) = UnivType (gammaAnn t) (\v -> replace n v t)
              | otherwise = t
          
          replace n v (FunType a fun args) = FunType a (replace n v fun) (fmap (replace n v) args)
          replace n v t@(VarType a m) | n == m    = a <$ v
                                      | otherwise = t
          replace n v (UnivType a f) = UnivType a (fmap (replace n v) f)
          replace _ _ t = t
          
          constrain ty (a, name, args) = BoundType a name args ty

instance GammaTypeable GammaDecl a where
    typecheck (VarDecl a bind expr) =
        do ety <- generalize $ typecheck expr
           bty <- typecheck bind
           unify bty ety
    typecheck (FunDecl a name binds ret stmts) =
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
           
           bindSym name fty

instance GammaTypeable GammaBind a where
    typecheck (PlainBind a sym) = createVar a >>= bindSym sym
    typecheck (TypeBind a sym ty) = bindSym sym ty

instance GammaTypeable GammaStmt a where
    typecheck (DeclStmt a decl) = typecheck decl
    typecheck (ExprStmt a expr) = typecheck expr
    typecheck (RetStmt a expr) = typecheck expr

instance GammaTypeable GammaExpr a where
    typecheck (LitExpr a (IntLit _)) = return (PrimType a CInt)
    typecheck (SymExpr a sym) = lookupSym a sym
    typecheck (TypeExpr a expr ty) =
        do ity <- typecheck expr
           unify ity ty
    typecheck (ApplyExpr a fun args) =
        do ret <- createVar a
           fty <- typecheck fun
           argty <- mapM typecheck args
           FunType _ rty _ <- unify fty (FunType a ret argty)
           return rty
