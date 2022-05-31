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
{-# LANGUAGE NoMonomorphismRestriction #-} -- ? testing
{-# LANGUAGE ImpredicativeTypes #-}
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

class  MiniScript a where
  type Mscript a

class BTCScript a where
  type BTCscript a

class  Policy a where
  type Compiler a = c | c -> a -- dsl for type level computation -- add a type family dependency
  -- add a type family dependency

data Geometry sign = Projection (MiniMapping sign)
data MiniMapping p = forall mini btc policy. p ~ policy => MMap (Compiler policy) (Mscript mini) (BTCscript btc)

-- (x :: (Determines a b (c :: Compiler p) => c ~ z) => Compiler p)
-- MMap x b a ~ c
-- p (c :: MiniMapping p)
-- Determines a b x
class (forall a b. (BTCScript b , MiniScript a, (Any => a), (Any => b)) => Determines a b ) => Mapping (x :: Compiler p) where
  compile :: Geometry (MMap x b c :: MiniMapping p) -> Mscript b -> BTCscript c




instance MiniScript "k" where
  type Mscript "k" = Minicons

data Minicons = T

instance BTCScript "k"  where
  type BTCscript "k" = BTCcons

data BTCcons = X | Y Int Float


-- (Policy a => Mapping a) => SecureMap a
-- mapping a = how to split tap tree multi sig
-- expensive verify?
