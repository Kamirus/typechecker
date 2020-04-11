module Test where

import Protolude hiding (const)
import Prelude (error)

import Language.Term
import Language.Type
import Language.TypeChecker.Types
import Language.TypeChecker.Typing
import Language.PPrint
import Test.Utils

id = "x" ~> "x"
id_ann = ("x" ~> "x") `eAnn` forAll "a" ("a" --> "a")
const = "x" ~> "y" ~> "x"
flip_const = "y" ~> "x" ~> "x"
const_ann = eAnn
  ("x" ~> "y" ~> "x")
  $ forAll "a" $ forAll "b" ("a" --> "b" --> "a")

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
  (ctx, aty) <- either (error . show) pure $ runMonadCheck $ typecheck e
  print $ ppCtx ctx
  print $ ppTerm e
  print $ ppAlgoType aty
