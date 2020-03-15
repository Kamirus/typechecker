-- Algorithmic subtyping
module Language.TypeChecker.SubTyping where

import Data.Fix

import Language.TypeChecker.Context
import Language.TypeChecker.Types

import Protolude

-- | Checks whether under input context `ctx`, type A is a subtype of B
-- | and returns an output context (delta)
subtype :: MonadCheck m => Context -> AlgoType -> AlgoType -> m Context
subtype ctx (Fix _A) (Fix _B) = go _A _B
  where
    -- | <: Var
    go (ATyVar tv) (ATyVar tv') | tv == tv', ctx `has` CtxTypeVar tv = pure ctx

    -- | <: Exvar
    go (AHatVar hv) (AHatVar hv') | hv == hv', ctx `has` CtxHatVar hv = pure ctx

    -- | <: ->
    go (ATyArrow a1 a2) (ATyArrow b1 b2) = do
      theta <- subtype ctx b1 a1
      subtype theta (substHv theta a2) (substHv theta b2)

    -- | <: ForallR
    go a (ATyForAll tv b) = do
      ctx' <- subtype (CtxTypeVar tv +: ctx) (Fix a) b
      tv `assertIn` ctx' $ do
        (_, _, delta) <- ctx' `hole` (CtxTypeVar tv)
        pure delta

    -- | <: ForallL
    -- | `forall tv. a` is a subtype of `b`
    -- | if we can instantiate `tv` with a monotype `tau`
    -- | such that `[tau / tv]a` is a subtype of `b`
    go (ATyForAll tv a) b = do
      -- We are not guessing so instantiate with a fresh existential variable
      hv <- freshHv tv
      let
        a' = (hv :/ tv) `substTv` a
        marker = CtxScopeMarker hv
      ctx' <- subtype (CtxHatVar hv +: marker +: ctx) a' (Fix b)
      marker `assertIn` ctx' $ do
        (_, _, delta) <- ctx' `hole` marker
        pure delta

    -- | <: InstantiateR
    go a (AHatVar hv) = do
      checkBeforeInstantiate a hv
      instantiate ctx (Fix a) (atyHatVar hv)

    -- | <: InstantiateL
    go (AHatVar hv) b = do
      checkBeforeInstantiate b hv
      instantiate ctx (atyHatVar hv) (Fix b)

    go a b = throw $ "subtype unreachable with :" <> show a <> " <: " <> show b

    checkBeforeInstantiate a hv = do
      _ <- hv `assertIn` ctx $ ctx `hole` CtxHatVar hv
      assert $ hv `NotIn` avHatVars (allVars $ Fix a)

assertIn :: (MonadCheck m, Show a, Show b) => a -> b -> Maybe c -> m c
assertIn a b = maybe (a `throwNotIn` b) pure


instantiate :: MonadCheck m => Context -> AlgoType -> AlgoType -> m Context
instantiate = undefined
