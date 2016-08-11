{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stub module for Johan Tibell's style.
--
-- Documented here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>
--
-- Questions:
--
-- How to indent after a guarded alt/rhs?
-- How to indent let?
-- How to indent large ADT constructors types?

module HIndent.Styles.JohanTibell where

import Control.Monad hiding (forM_)
import Control.Monad.State.Class
import Data.Foldable (forM_)
import Data.Int
import Data.Maybe
import Data.Monoid
import HIndent.Pretty
import HIndent.Types
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)

--------------------------------------------------------------------------------
-- Style configuration

-- | A short function name.
shortName :: Int64
shortName = 10

-- | Empty state.
data State =
  State

-- | The printer style.
johanTibell :: Style
johanTibell =
  Style {styleName = "johan-tibell"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Style modeled from Johan's style guide here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>"
        ,styleInitialState = State
        ,styleExtenders =
           [Extender decl
           ,Extender match
           ,Extender context
           ,Extender typ
           ,Extender conDecl
           ,Extender exp
           ,Extender guardedRhs
           ,Extender rhs
           ,Extender stmt
           ,Extender fieldupdate
           ]
        ,styleDefConfig =
           defaultConfig {configMaxColumns = 80
                         ,configIndentSpaces = 4}
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Extenders

