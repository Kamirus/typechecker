-- Algorithmic subtyping
module Language.TypeChecker.SubTyping where

import Protolude

import Data.Fix

import Language.PPrint
import Language.Type
import Language.TypeChecker.Context
import Language.TypeChecker.Monad
import Language.TypeChecker.Types

-- | Checks whether under input context `ctx`, type A is a subtype of B
-- | and returns an output context (delta)
subtype :: MonadCheck m => Context -> AlgoType -> AlgoType -> m Context
subtype ctx (Fix _A) (Fix _B) = logInfoWithIndent msg $ go _A _B
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
      (_, _, delta) <- ctx' `hole_` CtxTypeVar tv
      pure delta

    -- | <: ForallL
    -- | `forall tv. a` is a subtype of `b`
    -- | if we can instantiate `tv` with a monotype `tau`
    -- | such that `[tau / tv]a` is a subtype of `b`
    go (ATyForAll tv a) b = do
      -- We are not guessing so instantiate with a fresh existential variable
      hv <- freshHv tv
      let
        a' = hv :/ tv `substTv` a
        marker = CtxScopeMarker hv
      ctx' <- subtype (CtxHatVar hv +: marker +: ctx) a' (Fix b)
      (_, _, delta) <- ctx' `hole_` marker
      pure delta

    -- | <: InstantiateR
    go a (AHatVar hv) = do
      checkBeforeInstantiate a hv
      instantiateR ctx (Fix a) hv

    -- | <: InstantiateL
    go (AHatVar hv) b = do
      checkBeforeInstantiate b hv
      instantiateL ctx hv (Fix b)

    go a b = throw $ "can't subtype :" <> show a <> " <: " <> show b

    checkBeforeInstantiate a hv = do
      ctx `assertHas` CtxHatVar hv
      assert $ hv `NotIn` avHatVars (allVars $ Fix a)

    msg = "subtype" <+> pp' ctx <+> pp' (Fix _A) <+> pp' (Fix _B)

instantiateL :: MonadCheck m => Context -> HatVar -> AlgoType -> m Context
instantiateL ctx alphaHv aty = logInfoWithIndent msg $ catchError
  -- | InstLSolve
  (algoToMonoType' aty >>= instLSolve ctx alphaHv)
  $ const $ case unFix aty of
    -- | InstLReach
    AHatVar betaHv -> instLReach ctx alphaHv betaHv

    -- | InstLAIIR
    ATyForAll betaTv b -> do
      ctx `assertHas` CtxHatVar alphaHv
      instantiateL (CtxTypeVar betaTv +: ctx) alphaHv b

    -- | InstLArr
    ATyArrow b1 b2 -> do
      (hv1, hv2, ctx') <- setupInstArr ctx alphaHv
      theta <- instantiateR ctx' b1 hv1
      instantiateL theta hv2 (theta `substHv` b2)

    ATyVar _ -> throw "unreachable - type variable is a mono type"

  where msg = "instantiateL" <+> pp' ctx <+> pretty alphaHv <+> pp' aty

instantiateR ::  MonadCheck m => Context -> AlgoType -> HatVar -> m Context
instantiateR ctx aty alphaHv = logInfoWithIndent msg $ catchError
  -- | InstRSolve
  (algoToMonoType' aty >>= instLSolve ctx alphaHv)
  $ const $ case unFix aty of
    -- | InstRReach
    AHatVar betaHv -> instLReach ctx alphaHv betaHv

    -- | InstRAIIL
    ATyForAll betaTv b -> do
      ctx `assertHas` CtxHatVar alphaHv
      betaHv <- freshHv betaTv
      instantiateR
        (CtxTypeVar betaTv +: CtxScopeMarker betaHv +: ctx)
        (betaHv :/ betaTv `substTv` b)
        alphaHv

    -- | InstRArr
    ATyArrow a1 a2 -> do
      (hv1, hv2, ctx') <- setupInstArr ctx alphaHv
      theta <- instantiateL ctx' hv1 a1
      instantiateR theta (theta `substHv` a2) hv2

    ATyVar _ -> throw "unreachable - type variable is a mono type"

  where msg = "instantiateR" <+> pp' ctx <+> pp' aty <+> pretty alphaHv

instLSolve :: MonadCheck m => Context -> HatVar -> AlgoMonoType -> m Context
instLSolve ctx alphaHv tau = do
  (gamma', _, gamma) <- ctx `hole_` CtxHatVar alphaHv
  monoTypeWellFormed gamma tau
  pure $ gamma' <> alphaHv `eqConstraint` tau <> gamma

instLReach :: MonadCheck m => Context -> HatVar -> HatVar -> m Context
instLReach ctx alphaHv betaHv = do
  (ctx1, _, ctx2) <- ctx `hole_` CtxHatVar betaHv
  ctx2 `assertHas` CtxHatVar alphaHv
  pure $ ctx1 <> betaHv `eqConstraint` atyHatVar alphaHv <> ctx2

-- | Common util for both InstLArr and InstRArr
setupInstArr :: MonadCheck m => Context -> HatVar -> m (HatVar, HatVar, Context)
setupInstArr ctx alphaHv = do
  plug <- do
    (ctx1, _, ctx2) <- ctx `hole_` CtxHatVar alphaHv
    pure $ \newctx -> ctx1 <> newctx <> ctx2

  hv1 <- freshHv $ hvVar alphaHv -- freshRenamedHv (<> "1") alphaHv
  hv2 <- freshHv $ hvVar alphaHv -- freshRenamedHv (<> "2") alphaHv
  let
    ctx' = plug
      $ CtxConstraint alphaHv (hv1 `hvArrow` hv2)
      +: CtxHatVar hv1
      +: CtxHatVar hv2
      +: mempty
  pure (hv1, hv2, ctx')

algoToMonoType' :: MonadCheck m => AlgoType -> m AlgoMonoType
algoToMonoType' aty = maybe (throw "not mono") pure $ algoToMonoType aty

eqConstraint :: HatVar -> AlgoMonoType -> Context
hv `eqConstraint` tau = Ctx $ pure $ CtxConstraint hv tau

hvArrow :: HatVar -> HatVar -> AlgoMonoType
hv1 `hvArrow` hv2 = Fix $ AType $ TyArrow (atyHatVar hv1) (atyHatVar hv2)

-- freshRenamedHv :: MonadCheck m => (Text -> Text) -> HatVar -> m HatVar
-- freshRenamedHv f hv = freshHv $ TypeVar $ f $ fromTypeVar $ hvVar hv
