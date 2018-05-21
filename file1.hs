module A where


func01 :: Int -> Int
func01 x = 3 * x

import Control.Monad.State

data Exp = C Int | Exp :+ Exp | Exp :* Exp deriving Show

infixl 7 :*
infixl 6 :+

data Code = Push Int | Add | Mul deriving Show
type Program = [Code]

compile :: Exp -> Program
compile expr = reverse $ compile' expr []

compile' :: Exp -> Program -> Program
compile' (C x) xs = Push x : xs
compile' (r :+ l) xs = Add : compile' l (compile' r xs)
compile' (r :* l) xs = Mul : compile' l (compile' r xs)