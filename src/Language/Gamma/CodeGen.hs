{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

module Language.Gamma.CodeGen where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Writer
import Control.Lens
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Syntax

import Language.Gamma.Types
import Language.Gamma.Builtins

data GenError a = NotMonomorphic (GammaType a)
                deriving (Show)

type GenT ann m a = WriterT ([CExternalDeclaration ()], [CExternalDeclaration ()]) (ExceptT (GenError ann) m) a
type Gen ann a = GenT ann Identity a

runGenT :: (Functor m) => GenT ann m a -> m (Either (GenError ann) (a, CTranslUnit))
runGenT m = runExceptT (unitify <$> runWriterT m)
    where unitify (r, (a, b)) = (r, undefNode <$ CTranslUnit (a ++ b) ())

runGen :: Gen ann a -> Either (GenError ann) (a, CTranslUnit)
runGen = runIdentity . runGenT

emitFront :: (Monad m) => CExternalDeclaration () -> GenT ann m ()
emitFront d = tell ([d], [])

emitBack :: (Monad m) => CExternalDeclaration () -> GenT ann m ()
emitBack d = tell ([], [d])

class GammaGenable t a c | t -> a c where
    codegen :: (Monad m) => t -> GenT a m c

instance GammaGenable (GammaDecl a (a, GammaType a)) a (CDeclaration ()) where
    codegen (VarDecl (a, ty) bind expr) = do
      cty <- codegen ty
      cexpr <- codegen expr
      let ident = Ident (bind ^. gammaSym) 0 undefNode
      return (CDecl [CTypeSpec cty] [(Just (CDeclr (Just (ident)) [] Nothing [] ()), Just (CInitExpr cexpr ()), Nothing)] ())

instance GammaGenable (GammaType a) a (CTypeSpecifier ()) where
    codegen (PrimType a CInt) = return (CIntType ())
    codegen ty = throwError (NotMonomorphic ty)

instance GammaGenable (GammaExpr a (a, GammaType a)) a (CExpression ()) where
    codegen (LitExpr (a, ty) (IntLit i)) =
        return (CConst (CIntConst (CInteger i DecRepr (Flags 0)) ()))
