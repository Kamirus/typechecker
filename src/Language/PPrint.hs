{-# LANGUAGE FlexibleInstances #-}
module Language.PPrint
  ( module Language.PPrint
  , module Data.Text.Prettyprint.Doc
  ) where

import Protolude hiding (Type)

import Data.Fix (Fix (..), unFix)
import Data.Text.Prettyprint.Doc

import Control.Arrow ((>>>))
import Language.Term
import Language.Type
import Language.TypeChecker.Types

class PP a where
  pp :: a -> Doc ann
instance PP Type where pp = ppType
instance PP Term where pp = ppTerm
instance PP AlgoType where pp = ppAlgoType
instance PP Context where pp = ppCtx
instance PP ContextElem where pp = ppCtxEl

pp' :: PP a => a -> Doc ann
pp' a = "(" <> pp a <> ")"

ppType :: Type -> Doc ann
ppType = unFix >>> \case
  TyForAll tv a -> ppForAll tv a
  TyMono mty -> case mty of
    TyVar tv    -> pretty tv
    TyArrow a b -> ppArrow a b
  where
    ppForAll tv a = "∀" <+> pretty tv <> "." <+> ppType a
    ppArrow a b = ppWrapped a <+> "⟶" <+> ppType b
    ppWrapped = unFix >>> \case
      TyForAll tv a -> "(" <> ppForAll tv a <> ")"
      TyMono mty -> case mty of
        TyVar tv    -> pretty tv
        TyArrow a b -> "(" <> ppArrow a b <> ")"

ppTerm :: Term -> Doc ann
ppTerm = unFix >>> \case
  EVar v -> pretty v
  EAbs v e -> ppAbs v e
  EApp e e' -> ppApp e e'
  EAnn e a -> ppAnn e a
  where
    ppAbs v e  = "λ" <> pretty v <> "." <+> ppTerm e
    ppApp e e' = ppWrappedApp e <+> ppWrapped e'
    ppAnn e a  = ppWrapped e <+> "∷" <+> ppType a
    ppWrappedApp = unFix >>> \case
      e@EApp{} -> ppTerm $ Fix e
      e -> ppWrapped $ Fix e
    ppWrapped = unFix >>> \case
      EVar v -> pretty v
      EAbs v e -> "(" <> ppAbs v e <> ")"
      EApp e e' -> "(" <> ppApp e e' <> ")"
      EAnn e a -> "(" <> ppAnn e a <> ")"

ppAlgoType :: AlgoType -> Doc ann
ppAlgoType = unFix >>> \case
  ATyVar v -> pretty v
  AHatVar hv -> pretty hv
  ATyArrow a b -> ppArrow a b
  ATyForAll tv a -> ppForAll tv a
  where
    ppForAll tv a = "∀" <+> pretty tv <> "." <+> ppAlgoType a
    ppArrow a b = ppWrapped a <+> "⟶" <+> ppAlgoType b
    ppWrapped = unFix >>> \case
      ATyVar v -> pretty v
      AHatVar hv -> pretty hv
      ATyArrow a b -> "(" <> ppArrow a b <> ")"
      ATyForAll tv a -> "(" <> ppForAll tv a <> ")"

ppCtx :: Context -> Doc ann
ppCtx = foldl (\acc el -> ppCtxEl el <> "," <+> acc) mempty . getCtx

ppCtxEl :: ContextElem -> Doc ann
ppCtxEl = \case
  CtxTypeVar tv -> pretty tv
  CtxHatVar hv -> pretty hv
  CtxAnn v a -> pretty v <+> ":" <+> ppAlgoType a
  CtxConstraint hv mty -> pretty hv <+> "~" <+> ppAlgoType (monoToAlgoType mty)
  CtxScopeMarker hv -> "▶" <> pretty hv
