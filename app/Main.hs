{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad.State.Strict
import Prelude hiding (max)
import qualified Prelude as P (max)

main :: IO ()
main = do
--    print expr1
--    print $ eval expr1
--    print $ max expr1
    eval $ Stmts (FuncCall (Lit 34)) (FuncCall expr1)
    eval $ Stmts (VarAssign "x" (Lit 1)) (VarAssign "y" (Mul (Lit 2) (Lit 3)))

expr1 :: Expr Int
expr1 =
    Mul
        (Mul
            (Div
                (Add
                    (Lit 1)
                    (Lit 2))
                (Lit 3))
            (Lit 4))
        (Add
            (Add
                (Div
                    (Lit 56)
                    (Lit 7))
                (Lit 8))
            (Lit 9))

data REnv = REnv {
    renvVariables :: Map String RValue
}
data RValue = RVInt Int | RVUnit

class FromRValue a where
    fromRValue :: RValue -> Maybe a
class ToRValue a where
    toRValue :: a -> RValue

instance FromRValue Int where
    fromRValue (RVInt i) = Just i
    fromRValue _ = Nothing
instance FromRValue () where
    fromRValue RVUnit = Just ()
    fromRValue _ = Nothing
instance ToRValue Int where
    toRValue i = RVInt i
instance ToRValue () where
    toRValue _ = RVUnit

newtype RInt = RInt Int
newtype RUnit = RUnit

newtype Ruby a = Ruby { unRuby :: StateT REnv IO a }

runRuby :: Ruby a -> REnv -> IO a
runRuby ruby env = evalStateT (unRuby ruby) env

data Expr :: * -> * where
    Lit :: Int -> Expr RInt
    Add :: Expr RInt -> Expr RInt -> Expr RInt
    Sub :: Expr RInt -> Expr RInt -> Expr RInt
    Mul :: Expr RInt -> Expr RInt -> Expr RInt
    Div :: Expr RInt -> Expr RInt -> Expr RInt
    Stmts :: Expr b -> Expr r -> Expr r
    FuncCall :: RValue r => Expr r -> Expr RUnit
    VarAssign :: RValue r => String -> Expr r -> Expr RUnit
    VarRef :: RValue r => String -> Expr r

instance Show r => Show (Expr r) where
    show (Lit l) = "Lit " ++ show l
    show (Add e1 e2) = "Add (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Sub e1 e2) = "Sub (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Mul e1 e2) = "Mul (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Div e1 e2) = "Div (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Stmts e1 e2) = "Stmts (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (FuncCall e) = "FuncCall (" ++ show e ++ ")"
    show (VarAssign sym e) = "VarAssign \"" ++ sym ++ "\" (" ++ show e ++ ")"

eval :: REnv -> Expr r -> Ruby r
eval _ (Lit l) = return $ RInt l
eval _ (Add e1 e2) = (+) <$> (eval e1) <*> (eval e2)
eval _ (Sub e1 e2) = (-) <$> (eval e1) <*> (eval e2)
eval _ (Mul e1 e2) = (*) <$> (eval e1) <*> (eval e2)
eval _ (Div e1 e2) = div <$> (eval e1) <*> (eval e2)
eval _ (Stmts e1 e2) = eval e1 >> eval e2
eval _ (FuncCall e) = eval e >>= print
eval _ (VarAssign sym e) =
eval _ (VarRef sym) =

--max :: Expr -> Int
--max (Lit l) = l
--max (Add e1 e2) = P.max (max e1) (max e2)
--max (Sub e1 e2) = P.max (max e1) (max e2)
--max (Mul e1 e2) = P.max (max e1) (max e2)
--max (Div e1 e2) = P.max (max e1) (max e2)

