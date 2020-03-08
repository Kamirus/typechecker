module Language.TypeChecker.Context where

import Data.Fix

import Language.Type
import Language.Term
import Language.TypeChecker.Types
import qualified Language.Utils as Utils

import Protolude hiding (Type)

typeWellFormed :: forall m . MonadCheck m => Context -> AlgoType -> m ()
typeWellFormed = flip $ cata go
  where
    el `inCtx` ctx = when (isNothing $ hole ctx el) (el `throwNotIn` ctx)
    go :: AlgoTypeF (Context -> m ()) -> Context -> m ()
    go aty ctx = case aty of
      -- | ctx |- \hat{hv}
      -- | if \hat{hv} in ctx or there is a solved constraint with it
      AHatVar hv -> do
        let
          f = \case
            CtxHatVar hv' -> hv' == hv
            CtxConstraint hv' _ -> hv' == hv
            _ -> False
        when (isNothing $ holeWith f ctx) (hv `throwNotIn` ctx)
      AType ty -> case ty of
        -- | check with alpha added to the context
        TyForAll alpha k -> k $ CtxVar alpha +: ctx
        TyMono mty -> case mty of
          -- | type variable `alpha` should be in the context
          TyVar alpha -> CtxVar alpha `inCtx` ctx
          -- | case (A -> B) : simply check both types recursively
          TyArrow aty1 aty2 -> aty1 ctx >> aty2 ctx

infixr 5 +:
(+:) :: ContextElem -> Context -> Context
x +: xs = Ctx $ x : getCtx xs

-- | `hole gamma elem` finds the `elem` in the context `gamma`
-- | and breakes it into two parts: before and after the `elem`
-- |
-- | gamma1 ++ [elem] ++ gamma2
-- | -->
-- | (gamma1, elem, gamma2)
hole :: Context -> ContextElem -> Maybe (Context, ContextElem, Context)
hole gamma el = holeWith (== el) gamma

holeWith
  :: (ContextElem -> Bool) -> Context -> Maybe (Context, ContextElem, Context)
holeWith f (Ctx gamma) = aux <$> Utils.splitOn f gamma
  where aux (a, b, c) = (Ctx a, b, Ctx c)

notInWith
  :: (MonadError Text m, Show a)
  => (a -> ContextElem -> Bool)
  -> a
  -> Context
  -> m ()
notInWith f v ctx = when (isNothing $ holeWith (f v) ctx) (v `throwNotIn` ctx)

notIn :: MonadError Text m => ContextElem -> Context -> m ()
notIn = notInWith (==)

notInDomOf
  :: (MonadError Text m, ContextElemCaseOver a, Show a, Eq a)
  => a
  -> Context
  -> m ()
notInDomOf = notInWith $ \var -> caseOver (== var) $ const False

class ContextElemCaseOver a where
  caseOver :: forall r. (a -> r) -> (ContextElem -> r) -> ContextElem -> r

instance ContextElemCaseOver HatVar where
  caseOver f k = \case
    CtxHatVar hv -> f hv
    CtxConstraint hv _ -> f hv
    CtxScopeMarker hv -> f hv
    el -> k el

instance ContextElemCaseOver Var where
  caseOver f k = \case
    CtxAnn v _ -> f v
    el -> k el

instance ContextElemCaseOver TypeVar where
  caseOver f k = \case
    CtxVar alpha -> f alpha
    el -> k el

_throwWithBin :: (MonadError Text m, Show a, Show b) => Text -> a -> b -> m ()
_throwWithBin msg x xs = throw $ show x <> msg <> show xs

throwNotIn :: (MonadError Text m, Show a, Show b) => a -> b -> m ()
throwNotIn = _throwWithBin " not in "

throwIn :: (MonadError Text m, Show a, Show b) => a -> b -> m ()
throwIn = _throwWithBin " shouldn't be in "
