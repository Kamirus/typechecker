{-# LANGUAGE GADTs, FlexibleInstances #-}
module Language.TypeChecker.Context where

import Control.Category ((>>>))
import Data.Fix
import Data.Set as S
import Prelude (error)

import Language.Type
import Language.TypeChecker.Types
import Language.Utils (splitOn, unreachablePattern)

import Protolude hiding (Type)

-- | Under context \Gamma, type A is well-formed
typeWellFormed :: MonadCheck m => Context -> AlgoType -> m ()
typeWellFormed = flip $ cata $ goAlgoWellFormed goTypeWellFormed

monoTypeWellFormed :: MonadCheck m => Context -> AlgoMonoType -> m ()
monoTypeWellFormed = flip $ cata $ goAlgoWellFormed goMonoWellFormed

goAlgoWellFormed
  :: MonadCheck m
  => (f r -> Context -> m ())
  -> AlgoTypeF f r
  -> Context
  -> m ()
goAlgoWellFormed k aty = case aty of
  AHatVar hv -> \ctx -> do
    let
      f = \case
        CtxHatVar hv' -> hv' == hv
        CtxConstraint hv' _ -> hv' == hv
        _ -> False
    when (isNothing $ holeWith f ctx) (hv `throwNotIn` ctx)
  AType ty -> k ty

goTypeWellFormed :: MonadCheck m => TypeF (Context -> m ()) -> Context -> m ()
goTypeWellFormed = \case
    -- | check with alpha added to the context
  TyForAll alpha k -> \ctx -> k $ CtxTypeVar alpha +: ctx
  TyMono mty -> goMonoWellFormed mty

goMonoWellFormed
  :: MonadCheck m => MonoTypeF (Context -> m ()) -> Context -> m ()
goMonoWellFormed mty ctx = case mty of
    -- | type variable `alpha` should be in the context
  TyVar alpha -> CtxTypeVar alpha & inCtx
  -- | case (A -> B) : simply check both types recursively
  TyArrow aty1 aty2 -> aty1 ctx >> aty2 ctx
  where inCtx el = when (isNothing $ hole ctx el) (el `throwNotIn` ctx)


-- | Algorithmic context \Gamma is well-formed
contextWellFormed :: forall m . MonadCheck m => Context -> m ()
contextWellFormed = go . getCtx
  where
    go :: [ContextElem] -> m ()
    go = \case
      [] -> pure ()
      el : gamma -> do
        let AllVars tvs vs hvs = allVars $ Ctx gamma
        case el of
          -- | UvarCtx
          CtxTypeVar tv -> assert $ tv `NotIn` tvs

          -- | VarCtx
          CtxAnn v aty -> do
            assert $ v `NotIn` vs
            Ctx gamma `typeWellFormed` aty

          -- | EVarCtx
          CtxHatVar hv -> assert $ hv `NotIn` hvs

          -- | SolvedEVarCtx
          CtxConstraint hv algoMonoTy -> do
            assert $ hv `NotIn` hvs
            Ctx gamma `monoTypeWellFormed` algoMonoTy

          -- | MarkerCtx
          CtxScopeMarker hv -> do
            assert $ hv `NotIn` hvs
            assert $ Ctx gamma `NoHole` CtxScopeMarker hv

        -- | Common recursive check for all the cases, better to check it last
        go gamma


-- | Apply the context as a substitution to a type.
-- | only existential variables are affected
class SubstitutionTo a where
  substHv :: Context -> a -> a

instance SubstitutionTo AlgoType where
  substHv = cata . goSubstToAlgo

-- | Apply the context as a substitution to a type
goSubstToAlgo :: Context -> AlgoTypeF TypeF AlgoType -> AlgoType
goSubstToAlgo ctx = \case
  -- | lookup \hat{alpha} in the context
  AHatVar hv -> case findSolutionTo hv ctx of
    -- | found a solved constraint: replace \hat{alpha} with its solution
    Just mty -> monoToAlgoType mty
    -- | not found, but do a sanity check that \hat{alpha} is in the context
    Nothing -> assertInCtxAndReturn hv
  -- | Recursive case
  ATyForAll tv r -> atyForAll tv r
  ATyVar tv -> atyVar tv
  ATyArrow ra rb -> ra `atyArrow` rb
  _ -> unreachablePattern
  where
    assertInCtxAndReturn hv = case ctx `hole` CtxHatVar hv of
      Just _ -> Fix $ AHatVar hv
      Nothing -> error $ "unreachable: " <> show hv <> " not in " <> show ctx


infix :/
data SubstTv = HatVar :/ TypeVar

substTv :: SubstTv -> AlgoType -> AlgoType
substTv (hv :/ tv) = cata go
  where
    go :: AlgoTypeF TypeF AlgoType -> AlgoType
    go = \case
      a@(ATyVar tv')
        | tv == tv' -> atyHatVar hv
        | otherwise -> Fix a
      a@AHatVar{} -> Fix a
      -- | Recursive case
      ATyForAll tv' r -> atyForAll tv' r
      ATyArrow ra rb -> ra `atyArrow` rb
      _ -> unreachablePattern


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
holeWith f (Ctx gamma) = aux <$> splitOn f gamma
  where aux (a, b, c) = (Ctx a, b, Ctx c)

has :: Context -> ContextElem -> Bool
ctx `has` el = isJust $ hole ctx el

notHas :: Context -> ContextElem -> Bool
notHas ctx = not . has ctx

-- | Looks for a solved constraint  \hat{alpha} = tau  in the context
findSolutionTo :: HatVar -> Context -> Maybe AlgoMonoType
findSolutionTo hv = getCtx >>> \case
  [] -> Nothing
  el : gamma -> case el of
    CtxConstraint hv' mty | hv == hv' -> Just mty
    _ -> findSolutionTo hv $ Ctx gamma

data CtxAssert where
  NotIn ::(Show a, Ord a) => a -> Set a -> CtxAssert
  NoHole ::Context -> ContextElem -> CtxAssert
  -- ^ `NoHole ctx el` asserts that there is no element `el` in the `ctx` 

assert :: MonadCheck m => CtxAssert -> m ()
assert = \case
  a `NotIn` s -> when (S.member a s) (throwIn a s)
  ctx `NoHole` el -> when (isJust $ hole ctx el) (throwIn el ctx)

_throwWithBin :: (MonadError Text m, Show a, Show b) => Text -> a -> b -> m c
_throwWithBin msg x xs = throw $ show x <> msg <> show xs

throwNotIn :: (MonadError Text m, Show a, Show b) => a -> b -> m c
throwNotIn = _throwWithBin " not in "

throwIn :: (MonadError Text m, Show a, Show b) => a -> b -> m c
throwIn = _throwWithBin " shouldn't be in "
