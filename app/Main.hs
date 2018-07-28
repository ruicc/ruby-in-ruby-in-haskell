{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.List (intersperse)
import Data.Map.Strict as Map
import Control.Monad
import Lib

main = do
    putStrLn " * Calculate num"
    void $ runRuby emptyREnv $ eval $ Stmts [FuncCall "p" [Lit 34], FuncCall "p" [expr1]]
    putStrLn " * VarAssign/VarRef"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ VarAssign "x" (Lit 1)
        , VarAssign "y" (Add (Lit 2) (VarRef "x"))
        , FuncCall "p" [VarRef "y"]
        ]
    putStrLn " * While"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ VarAssign "i" (Lit 0)
        , While
            (Lt (VarRef "i") (Lit 10))
            (Stmts
                [ FuncCall "p" [VarRef "i"]
                , VarAssign "i" (Add (VarRef "i") (Lit 1))
                ])
        ]
    putStrLn " * User function definition"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ FuncDef "add" ["x", "y"] (Add (VarRef "x") (VarRef "y"))
        , FuncCall "p" [FuncCall "add" [Lit 1, Lit 2]]
        ]
    putStrLn " * Function scope"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ FuncDef "foo" [] (Stmts [VarAssign "x" (Lit 0), FuncCall "p" [VarRef "x"]])
        , VarAssign "x" (Lit 1)
        , FuncCall "foo" []
        , FuncCall "p" [VarRef "x"]
        ]
    putStrLn " * fib"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ FuncDef "fib" ["x"] (If (Lt (VarRef "x") (Lit 2)) (VarRef "x") (Add
                    (FuncCall "fib" [Sub (VarRef "x") (Lit 1)])
                    (FuncCall "fib" [Sub (VarRef "x") (Lit 2)])
                )
            )
        , FuncCall "p" [FuncCall "fib" [Lit 5]]
        , FuncCall "p" [FuncCall "fib" [Lit 7]]
        , FuncCall "p" [FuncCall "fib" [Lit 9]]
        ]

expr1 :: Expr
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


