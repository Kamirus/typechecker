module Language.TypeChecker.Types where

import Data.Fix

import Language.Type
import Language.Term

import Protolude hiding (Type)

-- | Types in the algorithmic system
data AlgoTypeF r
  = AType (TypeF r)
  | AHatVar HatVar
  deriving (Eq, Show, Functor)

type AlgoType = Fix AlgoTypeF

-- | Monotypes in the algorithmic system
data AlgoMonoTypeF r
  = AMono (MonoTypeF r)
  | AMHatVar HatVar
  deriving (Eq, Show, Functor)

type AlgoMonoType = Fix AlgoMonoTypeF

-- | Existential / Unification variable, denoted e.g. \hat{alpha}
data HatVar = HatVar
  { hvVar :: Var
  , hvUID :: Int
  } deriving (Eq, Show)

-- | Context is an ordered structure - list, without duplicates.
-- |
-- | List order is somewhat reversed compared to the contexts in the paper:
-- | `(x : alpha) :: alpha :: Gamma`  is equiv. to  `Gamma,alpha,(x:alpha)`
newtype Context = Ctx { getCtx :: [ContextElem] }
  deriving (Eq, Show)

-- | What can appear in the algorithmic context
data ContextElem
  -- | Type variable
  = CtxVar TypeVar
  -- | Existential type variable
  | CtxHatVar HatVar
  -- | Type annotation e.g. (x : A)
  | CtxAnn Var AlgoType
  -- | Equality constraint e.g. \hat{alpha} = tau
  | CtxConstraint HatVar AlgoMonoType
  -- | Scope marker of the existential variable
  | CtxScopeMarker HatVar
  deriving (Eq, Show)

type MonadCheck m = (MonadError Text m)

throw :: MonadCheck m => Text -> m ()
throw = throwError
