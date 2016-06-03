-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Substitution (
    -- * Reexported modules
    module X
) where

import Data.Rewriting.Substitution.Type as X hiding (fromMap, toMap)
import Data.Rewriting.Substitution.Ops as X
import Data.Rewriting.Substitution.Match as X
import Data.Rewriting.Substitution.Unify as X
import Data.Rewriting.Substitution.Pretty as X
import Data.Rewriting.Substitution.Parse as X
