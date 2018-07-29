
import Lib
import Control.Monad

main = do
    src <- readFile "./test.rb"
    let expr = parse src
    runRuby emptyREnv $ eval expr

test' :: IO ()
test' = do
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
        [ FuncDef "fib" ["x"] (If (LtEq (VarRef "x") (Lit 1)) (VarRef "x") (Add
                    (FuncCall "fib" [Sub (VarRef "x") (Lit 1)])
                    (FuncCall "fib" [Sub (VarRef "x") (Lit 2)])
                )
            )
        , FuncCall "p" [FuncCall "fib" [Lit 5]]
        , FuncCall "p" [FuncCall "fib" [Lit 7]]
        , FuncCall "p" [FuncCall "fib" [Lit 9]]
        ]
    putStrLn " * Array assign"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ VarAssign "ary" (AryNew [Lit 3, Lit 5, Lit 7])
        , AryAssign (VarRef "ary") (Lit 0) (Lit 42)
        , FuncCall "p" [AryRef (VarRef "ary") (Lit 0)]
        , FuncCall "p" [VarRef "ary"]
        ]
    putStrLn " * Hash assign"
    void $ runRuby emptyREnv $ eval $ Stmts
        [ VarAssign "hash" (HashNew [(Lit 0, Lit 3), (Lit 1, Lit 4)])
        , AryAssign (VarRef "hash") (Lit 0) (Lit 43)
        , FuncCall "p" [AryRef (VarRef "hash") (Lit 0)]
        , FuncCall "p" [VarRef "hash"]
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


