{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
module Test.Utils where

import Protolude hiding (Type)

import Language.Term
import Language.Type

infixr 1 -->
-- (-->) :: Type -> Type -> Type
(-->) :: (IsType a, IsType b) => a -> b -> Type
a --> b = toType a `tyArrow` toType b

forAll :: IsType a => Text -> a -> Type
forAll tv a = tyForAll (TypeVar tv) (toType a)

infixl 9 @@
(@@) :: (IsTerm a, IsTerm b) => a -> b -> Term
e @@ e' = toTerm e `eApp` toTerm e'

infixr 1 ~>
(~>) :: IsTerm a => Var -> a -> Term
v ~> e = v `eAbs` toTerm e

var :: Text -> Term
var = eVar . Var

x = var "x"
y = var "y"
z = var "z"


class IsType a where
  toType :: a -> Type

instance IsType Type where
  toType = identity

instance a ~ Text => IsType a where
  toType = tyVar . TypeVar


class IsTerm a where
  toTerm :: a -> Term

instance IsTerm Term where
  toTerm = identity

instance a ~ Text => IsTerm a where
  toTerm = eVar . Var

