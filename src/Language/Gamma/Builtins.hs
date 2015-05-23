module Language.Gamma.Builtins where

import Language.Gamma.Types

data GammaBuiltin = Builtin
    { builtinNameSimple :: GammaSym
    , builtinType :: GammaType ()
    }

builtinName :: GammaBuiltin -> GammaSym
builtinName b = "__builtin_" ++ builtinNameSimple b

funType :: GammaPrimType -> [GammaPrimType] -> GammaType ()
funType ret args = FunType () (PrimType () ret) (fmap (PrimType ()) args)

builtins :: [GammaBuiltin]
builtins =
    [ Builtin "cint_plus" (funType CInt [CInt, CInt])
    , Builtin "zero" (UnivType () (ConstrainedType () "Zero" [BoundType () 0] (BoundType () 0)))
    ]
