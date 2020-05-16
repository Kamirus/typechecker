module Test where

import Protolude hiding (const, log, fst, snd)
import Prelude (error)

import Language.Term
import Language.Type
import Language.TypeChecker.Types
import Language.TypeChecker.Typing
import Language.TypeChecker.Monad
import Language.PPrint
import Test.Utils

id = "x" ~> "x"
id_ann = ("x" ~> "x") `eAnn` forAll "a" ("a" --> "a")
const = "x" ~> "y" ~> "x"
flip_const = "y" ~> "x" ~> "x"

omega = "x" ~> "x" @@ "x"
omegaTy1 = eAnn
  ("x" ~> "x" @@ "x")
  $ (forAll "a" ("a" --> "a")) --> forAll "a" ("a" --> "a")

-- omegaTy2 x = (x :: forall a b. a -> b) x
-- NOT. Reason: `x : t0` and then `t0 ~ ∀ a b. a ⟶ b`
omegaTy2 = "x" ~> annX @@ "x"
  where annX = "x" `ann` (forAll "a" $ forAll "b" $ "a" --> "b")

impredTest1Ty = forAll "int"
  $   ((forAll "a" $ "a" --> "a") --> "int")
  --> (forAll "b" $ "int" --> "b" --> "b")
  --> "int"
  --> "int"
impredTest1 = ann
  ("foo" ~> "bar" ~> "x" ~> "foo" @@ ("bar" @@ "x"))
  $ impredTest1Ty
impredTest2 = ann
  ("compose" ~> "foo" ~> "bar" ~> "compose" @@ "foo" @@ "bar")
  $ forAll "p"
  $ forAll "q"
  $ forAll "r"
  $ (("q" --> "r") --> ("p" --> "q") --> "p" --> "r")
    --> impredTest1Ty


-- data Nat = Suc Nat | Zero
tyNat = forAll "r" $ ("r" --> "r") --> "r" --> "r"

natZero' = "s" ~> "z" ~> "z"
natZero = natZero' `ann` tyNat

natSuc' = "n" ~> "s" ~> "z" ~> "s" @@ ("n" @@ "s" @@ "z")
natSucWrongAnnot = natSuc' `ann` tyNat
natSuc = natSuc' `ann` tyNat --> tyNat

natMul' = "n1" ~> "n2" ~> "s" ~> "z" ~> "n1" @@ ("n" ~> "n2" @@ "s" @@ "n") @@ "z"
natMul = natMul' `ann` tyNat --> tyNat --> tyNat

-- pred n = Nat \s z → fst_ $ unNat n (\p → pair (snd_ p) (s $ snd_ p)) (pair z z)
natPred' = "n" ~> "s" ~> "z" ~> fst @@
  ( ("n" `ann` tyNat)
      @@ ("p" ~> pair @@ (snd @@ "p") @@ ("s" @@ (snd @@ "p")))
      @@ (pair @@ "z" @@ "z"))
natPred = natPred' `ann` tyNat --> tyNat

--      ∷ ∀ a b r. NAT → (PAIRR a b r → PAIRR a b r) → PAIRR a b r → PAIRR a b r
instNat = ann ("n" ~> "n") $ forAll "a" $ forAll "b" $ forAll "r"
  $ tyNat --> (typ --> typ) --> typ --> typ
  where typ = tyPairR "a" "b" "r"
--       ∷ ∀ a b. NAT → (PAIR a b → PAIR a b) → PAIR a b → PAIR a b
instNat' = ann ("n" ~> "n") $ forAll "a" $ forAll "b"
  $ tyNat --> (typ --> typ) --> typ --> typ
  where typ = tyPair "a" "b"

-- newtype Pair a b = Pair (∀ r. (a → b → r) → r)
tyPair a b = forAll "r" $ a --> b --> "r"
tyPairR a b r = a --> b --> r

pair' = "x" ~> "y" ~> "f" ~> "f" @@ "x" @@ "y"
pair = ann pair' $ forAll "a" $ forAll "b" $ tyPair "a" "b"

-- fst ∷ ∀ a b. Pair a b → a
fst' = "r" ~> "r" @@ ("x" ~> "y" ~> "x")
fst = ann fst' $ forAll "a" $ forAll "b" $ tyPair "a" "b" --> "a"

-- snd ∷ ∀ a b. Pair a b → b
snd' = "r" ~> "r" @@ ("x" ~> "y" ~> "y")
snd = ann snd' $ forAll "a" $ forAll "b" $ tyPair "a" "b" --> "b"
--------------------------------------------------------------------------------
-- Rank 2 Types
--------------------------------------------------------------------------------

-- test1 :: (forall a. (a -> a)) -> Number
-- test1 = \f -> f 0.0

-- forever :: forall m a b. (forall a b. m a -> (a -> m b) -> m b) -> m a -> m b
-- forever = \bind action -> bind action $ \_ -> forever bind action

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

go :: Term -> IO ()
go e = do
  print $ ppLog log
  (ctx, aty, ty) <- either (error . show) pure m
  print $ ppTerm e
  print $ ppCtx ctx
  print $ ppAlgoType aty
  print $ ppType ty
  where
    (m, log) = runMonadCheck $ do
      (ctx, aty) <- typecheck e
      ty <- generalizeTopLevel aty
      pure (ctx, aty, ty)
