{-# LANGUAGE FlexibleInstances #-}
module Language.TypeChecker.Types where

import Data.Fix
import qualified Data.Set as S
import Lens.Micro.Platform

import Language.Type
import Language.Term

import Protolude hiding (Type)

-- | Types in the algorithmic system
data AlgoTypeF f r
  = AType (f r)
  | AHatVar HatVar
  deriving (Eq, Show, Functor)

-- | Polytypes in the algorithmic system
type AlgoType = Fix (AlgoTypeF TypeF)

-- | Monotypes in the algorithmic system
type AlgoMonoType = Fix (AlgoTypeF MonoTypeF)

-- | Existential / Unification variable, denoted e.g. \hat{alpha}
data HatVar = HatVar
  { hvVar :: TypeVar
    -- ^ tracks the name of the original type variable
  , hvUID :: Int
    -- ^ unique id of the existential variable
  } deriving (Show)

instance Eq HatVar where
  (==) = (==) `on` hvUID

instance Ord HatVar where
  (<=) = (<=) `on` hvUID

-- | Context is an ordered structure - list, without duplicates.
-- |
-- | List order is somewhat reversed compared to the contexts in the paper:
-- | `(x : alpha) :: alpha :: Gamma`  is equiv. to  `Gamma,alpha,(x:alpha)`
newtype Context = Ctx { getCtx :: [ContextElem] }
  deriving (Eq, Show)

-- | What can appear in the algorithmic context
data ContextElem
  -- | Type variable
  = CtxTypeVar TypeVar
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

class HasAllVars a where
  allVars :: a -> AllVars

instance HasAllVars Context where
  allVars = foldMap allVars . getCtx

instance HasAllVars ContextElem where
  allVars = \case
    CtxTypeVar tv -> singleton tv
    CtxHatVar hv -> singleton hv
    CtxAnn v algotype -> singleton v <> allVars algotype
    CtxConstraint hv algomono -> singleton hv <> allVars algomono
    CtxScopeMarker _ -> mempty

instance HasAllVars AlgoType where
  allVars = cata $ \case
    AHatVar hv -> singleton hv
    AType t -> goTypeAllVars t

instance HasAllVars AlgoMonoType where
  allVars = cata $ \case
    AHatVar hv -> singleton hv
    AType t -> goMonoAllVars t

goTypeAllVars :: TypeF AllVars -> AllVars
goTypeAllVars = \case
  TyMono mt -> goMonoAllVars mt
  TyForAll tv s -> delete tv s

goMonoAllVars :: MonoTypeF AllVars -> AllVars
goMonoAllVars = \case
  TyVar tv -> singleton tv
  TyArrow s1 s2 -> s1 <> s2

-- | `AllVars` contains sets of:
-- |  - type variables: alpha's
-- |  - term variables: x's
-- |  - existential type variables: \hat{alpha}'s
data AllVars = AllVars
  { avTypeVars :: Set TypeVar
  , avVars :: Set Var
  , avHatVars :: Set HatVar
  } deriving (Eq, Show)

_avTypeVars :: Lens' AllVars (Set TypeVar)
_avTypeVars = lens avTypeVars (\av x -> av { avTypeVars = x })

_avVars :: Lens' AllVars (Set Var)
_avVars = lens avVars (\av x -> av { avVars = x })

_avHatVars :: Lens' AllVars (Set HatVar)
_avHatVars = lens avHatVars (\av x -> av { avHatVars = x })

instance Semigroup AllVars where
  (AllVars tvs vs hvs) <> (AllVars tvs' vs' hvs') = AllVars tvs'' vs'' hvs''
    where
      tvs'' = tvs <> tvs'
      vs'' = vs <> vs'
      hvs'' = hvs <> hvs'

instance Monoid AllVars where
  mempty = AllVars S.empty S.empty S.empty

-- | Type class for overloaded operations on each set of the `AllVars`
class AllVarsSet a where
  modifySet :: (Set a -> Set a) -> AllVars -> AllVars

  insert, delete :: Ord a => a -> AllVars -> AllVars
  insert x = modifySet $ S.insert x
  delete x = modifySet $ S.delete x

  singleton :: Ord a => a -> AllVars
  singleton = (`insert` mempty)

instance AllVarsSet TypeVar where
  modifySet f = _avTypeVars %~ f

instance AllVarsSet Var where
  modifySet f = _avVars %~ f

instance AllVarsSet HatVar where
  modifySet f = _avHatVars %~ f
