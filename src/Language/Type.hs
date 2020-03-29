module Language.Type where

import Protolude hiding (Type)

import Data.Fix (Fix (..))

-- | tau,sigma ::= x | tau -> sigma
data MonoTypeF r
  -- | Type variable
  = TyVar TypeVar
  -- | Function type
  | TyArrow r r
  deriving (Eq, Show, Functor)

-- | Monotypes
type MonoType = Fix MonoTypeF

-- | A,B,C ::= x | A -> B | forall x. A
data TypeF r
  -- | Reuse the structure from monotypes
  = TyMono (MonoTypeF r)
  -- | Universal quantifier
  | TyForAll TypeVar r
  deriving (Eq, Show, Functor)

-- | Types
type Type = Fix TypeF

tyVar :: TypeVar -> Type
tyVar = Fix . TyMono . TyVar

tyArrow :: Type -> Type -> Type
tyArrow t = Fix . TyMono . TyArrow t

tyForAll :: TypeVar -> Type -> Type
tyForAll v = Fix . TyForAll v

newtype TypeVar = TypeVar { fromTypeVar :: Text }
  deriving (Eq, Show, Ord)
