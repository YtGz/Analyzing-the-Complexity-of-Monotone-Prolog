-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Term (
    Term (..),
    -- * Reexported modules
    module X
) where

import Prelude ()
import Data.Rewriting.Term.Type as X
import Data.Rewriting.Term.Ops as X
import Data.Rewriting.Term.Pretty as X
import Data.Rewriting.Term.Parse as X
