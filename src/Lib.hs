module Lib where

import Language.Term
import Language.Type
import Language.TypeChecker.Types

import Data.Fix (Fix(..))
import Protolude hiding (Type)

infixr 1 ~>
(~>) :: Var -> Term -> Term
(~>) = eAbs

infixl 9 @@
(@@) :: Term -> Term -> Term
(@@) = eApp

infixr 1 -->
(-->) :: AlgoType -> AlgoType -> AlgoType
t1 --> t2 = Fix $ AType $ TyMono $ TyArrow t1 t2

someFunc :: IO ()
someFunc = putLText "someFunc"

tySome0 = a --> a --> b
tySome1 = forAll _b (a --> a --> b)
tySome2 = a --> forAll _b (a --> b)
tySome3 = a --> a --> forAll _b b

tyUnit = forAll _a a

forAll :: TypeVar -> AlgoType -> AlgoType
forAll tv = Fix . AType . TyForAll tv

a,b,c :: AlgoType
a = Fix $ AType $ TyMono $ TyVar _a
b = Fix $ AType $ TyMono $ TyVar _b
c = Fix $ AType $ TyMono $ TyVar _c

_a,_b,_c :: TypeVar
_a = TypeVar "a"
_b = TypeVar "b"
_c = TypeVar "c"

id,k,s :: Term
id = _x ~> x
k = _x ~> _y ~> x
s = _x ~> _y ~> _z ~> x @@ z @@ (y @@ z)

x,y,z :: Term
x = eVar _x
y = eVar _y
z = eVar _z

_x,_y,_z :: Var
_x = Var "x"
_y = Var "y"
_z = Var "z"
