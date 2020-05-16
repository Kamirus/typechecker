-- Algorithmic typing
module Language.TypeChecker.Typing where

import Protolude hiding (check, Type)

import Data.Fix

import Language.PPrint
import Language.Term
import Language.Type
import Language.TypeChecker.Context
import Language.TypeChecker.Monad
import Language.TypeChecker.SubTyping
import Language.TypeChecker.Types

typecheck :: MonadCheck m => Term -> m (Context, AlgoType)
typecheck e = do
  (ctx', a') <- infer mempty e
  logInfo $ pp' ctx'
  let ctx = solve ctx'
  let a = ctx `substHv` a'
  pure (ctx, a)

typecheck' :: MonadCheck m => Term -> m Type
typecheck' e = do
  (ctx, a) <- typecheck e
  logInfo $ pp' ctx
  logInfo $ pp' a
  generalizeTopLevel a

-- | Generalize unsolved hat vars renaming and binding them with forall.
-- | Safe to use only for top-level functions.
generalizeTopLevel :: MonadCheck m => AlgoType -> m Type
generalizeTopLevel a = withResetId $ do
  ctx <- foldM f mempty (avHatVars $ allVars a)
  let a' = ctx `substHv` a
  (foldr atyForAll a' $ avTypeVars $ allVars a')
    & algoToType
    & maybe (throw "generalize failed: present hatvar") pure
  where
    f ctx hv = do
      id <- freshId
      let tv = TypeVar $ "a" <> show id
      -- check tv is not in BTV(a)
      let cc = CtxConstraint hv $ amtyVar tv
      pure $ cc +: ctx

    withResetId m = do
      s <- get
      put $ (1, snd s)
      result <- m
      put s
      pure result

-- | Under input context Γ, e checks against input type A and outputs context ∆
check :: MonadCheck m => Context -> Term -> AlgoType -> m Context
check ctx _e@(Fix expr) _A@(Fix _a) = logInfoWithIndent msg $ go expr _a
  where
    -- | →I
    go (EAbs x e) (ATyArrow a b) = check (CtxAnn x a +: ctx) e b

    -- | ∀I
    go _ (ATyForAll tv a) = check (CtxTypeVar tv +: ctx) _e a

    -- | Sub - fallback to type synthesis
    go _ _ = do
      (theta, a) <- infer ctx _e
      subtype theta (theta `substHv` a) (theta `substHv` _A)

    msg = "check" <+> pp' ctx <+> pp' _e <+> pp' _A

-- | Under input context Γ, e synthesizes output type A and outputs context ∆
infer :: MonadCheck m => Context -> Term -> m (Context, AlgoType)
infer ctx _e@(Fix expr) = logInfoWithIndent msg $ case expr of
  -- | Var
  EVar v -> do
    a <- lookupVar' v ctx
    pure (ctx, a)

  -- | Anno
  EAnn e ty -> do
    let a = typeToAlgoType ty
    typeWellFormed ctx a
    delta <- check ctx e a
    pure (delta, a)

  -- | →I
  EAbs x e -> do
    hvA <- freshHv'
    hvB <- freshHv'
    let a = atyHatVar hvA
    let b = atyHatVar hvB
    let _A = a `atyArrow` b
    let x_hastype_hvA = CtxAnn x a
    let ctx' = x_hastype_hvA +: CtxHatVar hvB +: CtxHatVar hvA +: ctx
    (_, _, delta) <- check ctx' e b >>= (`hole_` x_hastype_hvA)
    pure (delta, _A)

  -- | →E
  EApp e1 e2 -> do
    (theta, a) <- infer ctx e1
    inferApp theta (theta `substHv` a) e2
  
  where msg = "infer" <+> pp' ctx <+> pp' _e

-- | Under input context Γ, applying a function of type A to e
-- | synthesizes type C and outputs context ∆
inferApp :: MonadCheck m => Context -> AlgoType -> Term -> m (Context, AlgoType)
inferApp ctx _A@(Fix _a) e = logInfoWithIndent msg $ go _a
  where
    -- | ∀App
    go (ATyForAll tv a) = do
      hv <- freshHv tv
      inferApp (CtxHatVar hv +: ctx) (hv :/ tv `substTv` a) e

    -- | →App
    go (ATyArrow a c) = do
      delta <- check ctx e a
      pure (delta, c)

    -- | \hat{α}App
    go (AHatVar hv) = do
      (hv1, hv2, ctx') <- setupInstArr ctx hv
      delta <- check ctx' e $ atyHatVar hv1
      pure (delta, atyHatVar hv2)

    go (ATyVar _) = throw "inferApp unreachable"

    msg = "inferApp" <+> pp' ctx <+> pp' _A <+> pp' e
