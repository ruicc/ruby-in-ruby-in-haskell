{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.IORef
import Data.List (intersperse)
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import IOClass

data REnv = REnv {
    genvVars :: Map String RFunc,
    lenvVars :: Map String RValue
}
data RValue
    = RInt !Int
    | RBool !Bool
    | RUnit
    | RAry !(VM.IOVector RValue)
    | RHash !(IORef (Map.Map Int RValue))

data RFunc
    = RBuiltIn String
    | RUserDefined [String] Expr
    deriving Show

instance EqIO RValue where
    eqIO (RInt x) (RInt y) = return $ x == y
    eqIO (RBool x) (RBool y) = return $ x == y
    eqIO RUnit RUnit = return True
    eqIO (RAry vx) (RAry vy) = if VM.length vx == VM.length vy
        then do
            bools <- forM [0 .. VM.length vx - 1] $ \ix -> do
                v1 <- VM.read vx ix
                v2 <- VM.read vy ix
                v1 `eqIO` v2
            return $ all id bools
        else return False
    eqIO (RHash hxref) (RHash hyref) = do
        hx <- readIORef hxref
        hy <- readIORef hyref
        bools <- forM (toList hx) $ \(k1,v1) -> do
            case Map.lookup k1 hy of
                Just v2 -> v1 `eqIO` v2
                Nothing -> return False
        return $ all id bools
    eqIO _ _ = return False

instance ShowIO RValue where
    showIO (RInt n) = return $ "RInt " ++ show n
    showIO (RBool b) = return $ "RBool " ++ show b
    showIO RUnit = return "RUnit"
    showIO (RAry vec) = do
        ls <- V.toList <$> V.freeze vec
        showList <- mapM showIO ls
        return $ "RAry [" ++ (concat $ intersperse "," showList) ++ "]"
    showIO (RHash hashRef) = do
        hash <- readIORef hashRef
        strList <- forM (toList hash) $ \(k,v) -> do
            str <- showIO v
            return $ "(" ++ show k ++ "," ++ str ++ ")"
        return $ "RHash [" ++ (concat $ intersperse "," strList) ++ "]"


emptyREnv :: REnv
emptyREnv = REnv genvVariables empty

genvVariables :: Map String RFunc
genvVariables = fromList [
        ("p", RBuiltIn "p")
    ]

newtype Ruby a = Ruby { unRuby :: StateT REnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState REnv)

runRuby :: REnv -> Ruby a -> IO a
runRuby env ruby = evalStateT (unRuby ruby) env

data Expr where
    Lit :: Int -> Expr
    Add :: Expr -> Expr -> Expr
    Sub :: Expr -> Expr -> Expr
    Mul :: Expr -> Expr -> Expr
    Div :: Expr -> Expr -> Expr
    Stmts :: [Expr] -> Expr
    FuncCall :: String -> [Expr] -> Expr
    VarAssign :: String -> Expr -> Expr
    VarRef :: String -> Expr
    Gt :: Expr -> Expr -> Expr
    Lt :: Expr -> Expr -> Expr
    GtEq :: Expr -> Expr -> Expr
    LtEq :: Expr -> Expr -> Expr
    Eq :: Expr -> Expr -> Expr
    If :: Expr -> Expr -> Expr -> Expr
    While :: Expr -> Expr -> Expr
    FuncDef :: String -> [String] -> Expr -> Expr
    AryNew :: [Expr] -> Expr
    AryRef :: Expr -> Expr -> Expr
    AryAssign :: Expr -> Expr -> Expr -> Expr
    HashNew :: [(Expr, Expr)] -> Expr
    deriving Show

callBuiltinFunction :: String -> [RValue] -> Ruby (Maybe RValue)
callBuiltinFunction fname args = case fname of
        "p" -> do
            stringList <- liftIO $ mapM showIO args
            liftIO (putStrLn (concat $ intersperse ", " stringList)) >> return (Just RUnit)
        _ -> return Nothing


eval :: Expr -> Ruby RValue
eval (Lit l) = return $ RInt l
eval (Add e1 e2) = (\(RInt x) (RInt y) -> RInt (x + y)) <$> (eval e1) <*> (eval e2) -- FIXME: Might throw RuntimeException!
eval (Sub e1 e2) = (\(RInt x) (RInt y) -> RInt (x - y)) <$> (eval e1) <*> (eval e2)
eval (Mul e1 e2) = (\(RInt x) (RInt y) -> RInt (x * y)) <$> (eval e1) <*> (eval e2)
eval (Div e1 e2) = (\(RInt x) (RInt y) -> RInt (x `div` y)) <$> (eval e1) <*> (eval e2)
eval (Stmts es) = go es
  where
    go [] = return RUnit
    go [e] = eval e
    go (e:es) = eval e >> go es
eval (FuncCall fname es) = do
    !env <- get
    args <- mapM eval es
    case Map.lookup fname (genvVars env) of
        Just rfunc -> case rfunc of
            RBuiltIn name -> do
                mret <- callBuiltinFunction name args
                case mret of
                    Just ret -> return ret
                    Nothing -> return RUnit
            RUserDefined argNames body -> do
                let !(newLenv :: Map String RValue) = fromList $ zip argNames args
                !oldEnv <- get
                put $ oldEnv { lenvVars = newLenv }
                !ret <- eval body
                !newEnv <- get
                -- Change lenv back to old one. `eval body` might change genvVars.
                put $ oldEnv { genvVars = genvVars newEnv }
                return ret
        Nothing -> return RUnit
eval (VarAssign sym e) = do
    rvalue <- (eval e)
    modify' $ \env ->
        let varsMap = lenvVars env
            !varsMap' = insert sym rvalue varsMap
        in env { lenvVars = varsMap' }
    return RUnit
eval (VarRef sym) = do
    !env <- get
    let varsMap = lenvVars env
        rvalue = Map.lookup sym varsMap
    return $ case rvalue of
            Just !rv -> rv
            Nothing -> RUnit
eval (Gt e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let n1 = case v1 of { RInt n -> n; _ -> error "Expected RInt" }
    let n2 = case v2 of { RInt n -> n; _ -> error "Expected RInt" }
    return $ RBool $ n1 > n2
eval (Lt e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let n1 = case v1 of { RInt n -> n; _ -> error "Expected RInt" }
    let n2 = case v2 of { RInt n -> n; _ -> error "Expected RInt" }
    return $ RBool $ n1 < n2
eval (GtEq e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let n1 = case v1 of { RInt n -> n; _ -> error "Expected RInt" }
    let n2 = case v2 of { RInt n -> n; _ -> error "Expected RInt" }
    return $ RBool $ n1 >= n2
eval (LtEq e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let n1 = case v1 of { RInt n -> n; _ -> error "Expected RInt" }
    let n2 = case v2 of { RInt n -> n; _ -> error "Expected RInt" }
    return $ RBool $ n1 <= n2
eval (Eq e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    b <- liftIO $ v1 `eqIO` v2
    return $ RBool b
eval (If ep e1 e2) = do
    p <- eval ep
    case p of
        RBool b -> if b then eval e1 else eval e2
        RInt _ -> error "Expected RBool, but actual RInt"
        RUnit -> error "Expected RBool, but actual RUnit"
        _ -> error "Expected RBool"
eval (While pe body) = loop
  where
    loop = do
        p <- eval pe
        case p of
            RBool b -> if b
                then eval body >> loop
                else return RUnit
            RInt _ -> error "Expected RBool, but actual RInt"
            RUnit -> error "Expected RBool, but actual RUnit"
            _ -> error "Expected RBool"
eval (FuncDef fname varNames body) = do
    modify' (\renv ->
        let genv :: Map String RFunc = genvVars renv
            !genv' = insert fname (RUserDefined varNames body) genv
        in renv { genvVars = genv' })
    return RUnit
eval (AryNew es) = do
    vs <- mapM eval es
    mvec <- liftIO $ V.thaw (V.fromList vs)
    return $ RAry mvec
eval (AryRef eAry eIdx) = do
    ary <- eval eAry
    idx <- eval eIdx
    let ix :: Int = case idx of
            RInt n -> n
            _ -> error "Expected RInt"
    case ary of
        RAry vec -> do
            -- TODO: out of range
            v <- liftIO $ VM.read vec ix
            return v
        RHash ref -> do
            hash <- liftIO $ readIORef ref
            case Map.lookup ix hash of
                Just v -> return v
                Nothing -> return RUnit
        _ -> error "Expected RAry"
eval (AryAssign eAry eIdx e) = do
    ary <- eval eAry
    idx <- eval eIdx
    v <- eval e
    let ix = case idx of
            RInt n -> n
            _ -> error "Expected RInt"
    case ary of
        RAry vec -> do
            liftIO $ VM.write vec ix v
            return RUnit
        RHash ref -> liftIO $ atomicModifyIORef' ref $ \hash ->
            let hash' = Map.insert ix v hash
            in (hash', RUnit)
        _ -> error "Expected RAry"
eval (HashNew es) = do
    kvs <- forM es $ \(ek,ev) -> do
        k <- eval ek
        let ix = case k of
                RInt n -> n
                _ -> error "Expected RInt"
        v <- eval ev
        return (ix,v)
    hsh <- liftIO $ newIORef $ Map.fromList kvs
    return $ RHash hsh


parse :: String -> Expr
parse _ = Stmts [FuncCall "p" [Lit 34], FuncCall "p" [expr2]]
  where
    expr2 =
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
