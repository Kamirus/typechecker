module Language.Term where

import Data.Fix (Fix(..))

import Language.Type (Type)

import Protolude hiding (Type)

-- | e ::= x | \x.e | e e | (e : A)
data TermF r
  -- | term variable
  = EVar Var
  -- | lambda abstraction
  | EAbs Var r
  -- | application
  | EApp r r
  -- | type annotation
  | EAnn r Type
  deriving (Eq, Show, Functor)

-- | Terms
type Term = Fix TermF

eVar :: Var -> Term
eVar = Fix . EVar

eAbs :: Var -> Term -> Term
eAbs v = Fix . EAbs v

eApp :: Term -> Term -> Term
eApp e = Fix . EApp e

eAnn :: Term -> Type -> Term
eAnn e ty = Fix $ EAnn e ty

newtype Var = Var { fromVar :: Text }
  deriving (Eq, Show, Ord)
