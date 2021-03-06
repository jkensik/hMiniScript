{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-} -- polykinds implies datakinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-} -- probably dont need but look it up
{-# LANGUAGE StandaloneKindSignatures #-} -- I might not need this
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- might hide an error
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-} -- ? testing
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import GHC.TypeLits
import GHC.Exts

-- linear types = stack frames
-- xa # nm -> b
-- sat/constraints = satisfaction
-- bindings intmap / type var of the env to be squashed?
-- instance resolution = eq (satisfaction-probably the same/dissatisfaction)

-- consume n stack elements
-- place n on stack
-- no satisfaction?
-- conditional disatisfaction?

-- how does it optimize based on the probabilities

-- TYPE info
-- satisfaction monotonic decreasing probability
-- fragment byte size to satisfy ie. witness and length specifiers in bits
-- to dissatisfy too

-- generic fields add types type -> Arc<Miniscript<Pk, Ctx>>,
-- create typed expression with a coproduct, types instanciated through env


-- allow typing of subexpressions properties
-- check after satisfy

-- types to ask about : terminal, Arc<Miniscript<Pk, Ctx>>
-- terminal are things without sub expressions, not cons trees
-- miniscript attaches to a terminal node, has a type of the expression has extra data to analyze
-- evolves a script/key with context passing
-- it take a proxy of the ctx type

-- have constraints imply coproduct changes or errors in the middle of compiling

-- tapscript sub expression instance, tap hash instance
-- musig instnace

-- correctness/ soundness policies

-- malleabiilty
--  * safe , can another party prove satisfaction before they have seen one
--  * non-malleable , does there exist a way to satisfy avoiding malleability
--  * if given an input that is is not nonmalleable it will be malleable
--  * unique disatisfaction implies there is only one 'fail' that pushes 0, which is always available
--  * unkown

-- exception monad
-- wrapping funcitons as objects inheriting traits

-- errorKind ?
-- node type modifiers

-- output signature maybe = satisfy if stack completes 1

main :: IO ()
main = print 5


-- where linear types , where sat/ bindings
-- class Determines a b x | a -> b, b -> a, x -> a, a -> x, x -> b ,b -> x
class Determines a b | a -> b, b -> a
-- a and b must be constraint? interesting
-- maybe add an annotation or a custom error?
class  MiniScript a where
  type Mscript a -- :: Constraint

class BTCScript a where
  type BTCscript a -- :: Constraint

class  Policy a where
  type Compiler a = c | c -> a -- dsl for type level computation -- add a type family dependency
  -- add a type family dependency , it worked

data Geometry sign = Projection (MiniMapping) -- use a closed type function to get a and b minimapping
data MiniMapping = forall policy. MMap (Compiler policy) -- use kinds to overlap type family instances

-- p ~ policy =>
-- (x :: (Determines a b (c :: Compiler p) => c ~ z) => Compiler p)
-- MMap x b a ~ c
-- p (c :: MiniMapping p)
-- Determines a b x
class ((Any => a, Any => b) => Determines a b) => Mapping x a b where
  compile :: Geometry (MMap x) -> Mscript a -> BTCscript b


instance (Determines a b) => Mapping (x :: Compiler "fees" ) a b where -- prove constraint
  compile stuff a = X -- undefined -- take type here
-- add a custom error to remind to prove injectivity
-- add a custom error to implement policy before mapping

instance Policy "fees" where
  type Compiler "fees" = LowFees

data LowFees = Fee

type family K :: Constraint

instance a ~ K => MiniScript a where -- change k to a constraint kind
  type Mscript a = Minicons

data Minicons = T

instance a ~ K => BTCScript a  where
  type BTCscript a = BTCcons

data BTCcons = X | Y Int Float


instance Policy "malleability" where
  type Compiler "malleability" = Segwit

data Segwit = O | N

type family Y :: Constraint -- something needs to wrap Y

instance a ~ Y => MiniScript a where
 type Mscript a = MCommute

data MCommute = M | C


instance (Determines a b) => Mapping (x :: Compiler "malleability" ) a b where -- prove constraint
  compile stuff a = _a -- undefined -- take type here









-- (Policy a => Mapping a) => SecureMap a
-- mapping a = how to split tap tree multi sig
-- expensive verify?


-- this approach allows you to enforce traits of data families
