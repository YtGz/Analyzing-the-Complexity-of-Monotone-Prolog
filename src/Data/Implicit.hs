{-
source: https://github.com/duairc/implicit-params
modified to work with instances of the Default type class created at runtime (via reflection)
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-|
"Data.Implicit" provides both named and unnamed implicit parameters that
support default values (given by the 'Default' class from the @data-default@
package). It makes no use of the @ImplicitParams@ extension and instead
everything is done using type classes.
Here is an example of unnamed implicit parameters:
@
{\-\# LANGUAGE FlexibleContexts #-\}
import "Data.Implicit"
putParam :: 'Implicit_' String => IO ()
putParam = putStrLn $ \"Param was: \" ++ show ('param_' :: String)
@
We define @putParam@, which is a simple function which takes an implicit
parameter of type @String@, and prints it to the screen. The 'param_' function
is used to retrieve the unnamed implicit parameter of type @String@ from
@putParam@'s context. The type signature is necessary to force 'param_' to
return a @String@, as this cannot be inferred due to the polymorphism of
@show@.
>>> putParam
Param was ""
This is how we call @putParam@ without specifying its implicit parameters. If
an implicit parameter is left unspecified, its value is defaulted to 'def',
assuming that its type has a 'Default' instance. If not, then it is a type
error not to specify the value of an implicit parameter.
>>> putParam $~ "hello, world"
Param was "hello, world"
The operator '$~' takes a function @f@ and a value to which to set the
homotypic implicit parameter on @f@. It applies the implicit parameter to @f@
and returns the result. There is also a prefix version of @$~@ whose arguments
are flipped called 'setParam_'.
Here is an example of named implicit parameters:
@
{\-\# LANGUAGE DataKinds, FlexibleContexts, RankNTypes #-\}
import "Data.Implicit"
import "Data.Proxy"
foo :: Proxy \"foo\"
foo = Proxy
bar :: Proxy \"bar\"
bar = Proxy
putFooBar :: ('Implicit' \"foo\" String, 'Implicit' \"bar\" String) => IO ()
putFooBar = do
    putStrLn $ \"foo was: \" ++ show (param foo :: String)
    putStrLn $ \"bar was: \" ++ show (param bar :: String)
@
The 'Implicit' constraint is the named equivalent of 'Implicit_'. It takes an
additional argument @s@ to specify the name of the implicit parameter.
Implicit parameters can be \"named\" using any type (of any kind, on compilers
that support the @PolyKinds@ extension). (The above code uses type-level
strings of the @Symbol@ kind from the "GHC.TypeLits" module, which is the
recommended way to name implicit parameters. However, @Symbol@ requires the
@DataKinds@ extension and at least version 7.8 of GHC (7.6 is broken; see GHC
bug \#7502), so you are free to use other types of other kinds if you want to
support older versions of GHC.) 'param' and 'setParam' work like their unnamed
counterparts 'param_' and 'setParam_', but they also take a proxy argument to
specify the name of the implicit parameter. The code above defines @foo@ and
@bar@ to hide away the (slightly ugly) proxy stuff.
>>> putFooBar
foo was: ""
bar was: ""
Once again, the defaults of unspecified implicit parameters are given by the
'Default' class.
>>> setParam foo "hello, world" putFooBar
foo was: "hello, world"
bar was: ""
>>> setParam bar "goodbye" $ setParam foo "hello, world" putFooBar
foo was: "hello, world"
bar was: "goodbye"
An infix version of @setParam@ is also provided, '~$'. Using @~$@, the above
example would be:
>>> putFooBar ~$ foo ~$ bar $$ "goodbye" $$ "hello, world"
foo was: "hello, world"
bar was: "goodbye
-}

module Data.Implicit
    ( Implicit
    , param
    , setParam
    , (~$)
    , (~..)
    , ($$)

    , Implicit_
    , param_
    , setParam_
    , ($~)
    , (~.)
    )
where

import           Data.Default.Class (Default, def)
import           System.IO.Unsafe (unsafePerformIO)
import           Unsafe.Coerce (unsafeCoerce)
import           Query.Utilities
import           System.Environment
import           Language.Prolog.Parser
import           ExprToTerm.Conversion
import           SymbolicEvaluationGraphs.Types
import           SymbolicEvaluationGraphs.Utilities

------------------------------------------------------------------------------
-- | The constraint @'Implicit' \"foo\" String@ on a function @f@ indicates
-- that an implicit parameter named @\"foo\"@ of type @String@ is passed to
-- @f@.
class Implicit s a where
    -- | 'param' retrieves the implicit parameter named @s@ of type @a@ from
    -- the context @'Implicit' s a@. The name @s@ is specified by a proxy
    -- argument passed to @param@.
    param :: proxy s -> a


------------------------------------------------------------------------------
instance Default a => Implicit s a where
    param _ = def


------------------------------------------------------------------------------
newtype Param s a b = Param (Implicit s a => b)


------------------------------------------------------------------------------
-- | 'setParam' supplies a value for an implicit parameter named @s@ to a
-- function which takes a homotypic and homonymous implicit parameter. The
-- name @s@ is specified by a proxy argument passed to @setParam@.
setParam :: forall a b proxy s. proxy s -> a -> (Implicit s a => b) -> b
setParam _ a f = unsafeCoerce (Param f :: Param s a b) (const a)
{-# INLINE setParam #-}


------------------------------------------------------------------------------
-- | An infix version of 'setParam' with flipped arguments.
(~$) :: forall a b proxy s. (Implicit s a => b) -> proxy s -> a -> b
infixl 1 ~$
(~$) f _ a = unsafeCoerce (Param f :: Param s a b) (const a)
{-# INLINE (~$) #-}


------------------------------------------------------------------------------
-- | Modify a named implicit parameter.
(~..) :: Implicit s a => (Implicit s b => c) -> proxy s -> (a -> b) -> c
(~..) f proxy g = f ~$ proxy $$ g (param proxy)
infixl 8 ~..
{-# INLINE (~..) #-}


------------------------------------------------------------------------------
-- | A left-associated version of '$'.
($$) :: (a -> b) -> a -> b
infixl 0 $$
($$) = id
{-# INLINE ($$) #-}


------------------------------------------------------------------------------
-- | The constraint @'Implicit_' String@ on a function @f@ indicates that an
-- unnamed implicit parameter of type @String@ is passed to @f@.
class Implicit_ a where
    -- | 'param_' retrieves the unnamed implicit parameter of type @a@ from
    -- the context @'Implicit_' a@.
    param_ :: a


------------------------------------------------------------------------------
--make program clauses available to every function as implicit parameter
instance Implicit_ [Clause] where
  {-# NOINLINE param_ #-} -- to ensure that I/O is done at most once
  param_ = unsafePerformIO readPrologFile

readPrologFile :: IO [Clause]
readPrologFile = do
  as <- getArgs
  (exprs,_) <- parseProlog2 "C:\\Users\\Philipp\\Documents\\Uni\\Bachelorarbeit\\code\\resources\\recursive_function_symbol_example.pl" --(head as)
  let queries = map (termToClause . exprToTerm) (filter (not . isQuery) exprs) in
    if hasNoAbstractVariables queries
      then return queries
      else error "Source program contains variables of the form \"T\" ++ [Int]. Try renaming."


------------------------------------------------------------------------------
newtype Param_ a b = Param_ (Implicit_ a => b)


------------------------------------------------------------------------------
-- | 'setParam_' supplies a value for an unnamed implicit parameter to a
-- function which takes a homotypic implicit parameter.
setParam_ :: forall a b. a -> (Implicit_ a => b) -> b
setParam_ a f = unsafeCoerce (Param_ f :: Param_ a b) a
{-# INLINE setParam_ #-}


------------------------------------------------------------------------------
-- | An infix version of 'setParam_' with flipped arguments.
($~) :: forall a b. (Implicit_ a => b) -> a -> b
infixl 1 $~
f $~ a = unsafeCoerce (Param_ f :: Param_ a b) a
{-# INLINE ($~) #-}


------------------------------------------------------------------------------
-- | Modify an unnamed implicit parameter.
(~.) :: Implicit_ a => (Implicit_ b => c) -> (a -> b) -> c
f ~. g = f $~ g param_
infixl 8 ~.
{-# INLINE (~.) #-}

{-
Copyright (c) 2013, Shane O'Brien

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Shane O'Brien nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
