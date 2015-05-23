{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Language.Gamma.Parser.Commented where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Proxy
import Text.Trifecta
import Text.Parser.Token.Style

-- a way to decide comment style based on a phantom type
class CommentedStyle t where
    commentedStyle :: Proxy t -> CommentStyle

-- a parser transformer that skips comments as whitespace
newtype Commented sty m a = Commented { runCommented :: m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, DeltaParsing, Errable, Monoid)

instance MonadTrans (Commented sty) where
    lift = Commented

instance (TokenParsing m, CommentedStyle sty) => TokenParsing (Commented sty m) where
  nesting (Commented m) = Commented (nesting m)
  semi      = Commented semi
  highlight h (Commented m) = Commented (highlight h m)
  someSpace = Commented (buildSomeSpaceParser someSpace style)
      where style = commentedStyle (Proxy :: Proxy sty)
