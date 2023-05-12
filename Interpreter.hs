module Interpreter where

import Data.Map ( insert, lookup, lookupMax, Map, fromList, empty, toList )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except
import Agol.Abs
import Data.Text.Array (new)

showPos :: BNFC'Position -> String
showPos Nothing = "Unknown position."
showPos (Just (line, col)) = "line " ++ show line ++ ", column " ++ show col ++ "."

data Value = TInt Integer | TBool Bool | TString String | TList [Value] | TTuple [Value] | TNone
    deriving (Eq, Show)

data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }
instance Show Env where
    show env = show (envVar env) ++ "\n"

data Fun = Fun { funArgs :: [Arg], funEnv :: Env, funBlock :: Block }
data Flags = Flags { loop_break :: Bool, loop_cont :: Bool }

type EnvVar = Map Ident Loc
type EnvFun = Map Ident Fun
type Loc = Integer
type Store = Map Loc Value

type ExecM a = ExceptT String (StateT Store (ReaderT Env IO)) a

emptyFlags :: Flags
emptyFlags = Flags { loop_break = False, loop_cont = False }

emptyEnv :: Env
emptyEnv = Env {envVar = Data.Map.empty, envFun = Data.Map.empty, envFlags = emptyFlags}

emptyStore :: Store
emptyStore = Data.Map.empty

myShow :: Value -> IO()
myShow (TInt x) = putStr (show x)
myShow (TBool x) = putStr (show x)
myShow (TString x) = putStr x

nextLoc :: Store -> Loc
nextLoc store = do
    case Data.Map.lookupMax store of
        Just (value, _) -> value + 1
        Nothing -> 0

varLoc :: Ident -> EnvVar -> Loc
varLoc ident env = do
    case Data.Map.lookup ident env of
        Just value -> value
        Nothing -> error "Unexpected failure! variable not found!"

locVal :: Loc -> Store -> Value
locVal loc store = do
    case Data.Map.lookup loc store of
        Just value -> value
        Nothing -> error "Unexpected failure! location not found!"

varVal :: Ident -> EnvVar -> Store -> Value
varVal ident env store = do
    let loc = varLoc ident env
    locVal loc store

addArg :: Arg -> Expr -> Env -> ExecM Env
addArg (ArgR _ typ ident) expr env = do
    case expr of
        ERef _ ref_ident -> do 
            let new_env = env { envVar = insert ident (varLoc ref_ident (envVar env))  (envVar env) }
            return new_env
        _ -> do
            value <- evalExpr expr
            store <- get
            let new_loc = nextLoc store
            let new_env = env { envVar = insert ident new_loc (envVar env) }
            modify (insert new_loc value)
            return new_env

addArgs :: [Arg] -> [Expr] -> Env -> ExecM Env
addArgs [] [] env = ask
addArgs (arg:args) (expr:exprs) env = do
    new_env <- addArg arg expr env
    local (const new_env) (addArgs args exprs new_env)

addVars :: [Item] -> ExecM Env
addVars [] = ask
addVars ((NoInit _ ident) : items) = do
    env <- ask
    store <- get
    let new_env = env { envVar = insert ident (nextLoc store) (envVar env) }
    local (const new_env) (addVars items)

addVars ((Init _ ident expr) : items) = do
    env <- ask
    store <- get
    value <- evalExpr expr
    let new_loc = nextLoc store
    let new_env = env { envVar = insert ident new_loc (envVar env) }
    modify (insert new_loc value)
    local (const new_env) (addVars items)

evalExpr :: Expr -> ExecM Value

evalExpr (ELitInt _ n) = return (TInt n)

evalExpr (ELitTrue _) = return (TBool True)

evalExpr (ELitFalse _) = return (TBool False)

evalExpr (EString _ str) = return (TString str)

evalExpr (EVar _ ident) = do
    env <- ask
    store <- get
    return $ varVal ident (envVar env) store

evalExpr (ERef _ ident) = do
    env <- ask
    return $ TInt (varLoc ident (envVar env))

evalExpr (Neg _ expr) = do
    TInt n <- evalExpr expr
    return (TInt (-n))

evalExpr (Not _ expr) = do
    TBool b <- evalExpr expr
    return (TBool (not b))

evalExpr (EMul pos expr1 mul_op expr2) = do
    TInt n1 <- evalExpr expr1
    TInt n2 <- evalExpr expr2
    case mul_op of
        Times _ ->  return $ TInt (n1 * n2)
        Div _ -> do
            if n2 == 0 then
                throwError ("Division by zero! Error at: " ++ showPos pos)
            else
                return $ TInt (n1 `div` n2)
        Mod _ -> return $ TInt (n1 `mod` n2)

evalExpr (EAdd _ expr1 add_op expr2) = do
    TInt n1 <- evalExpr expr1
    TInt n2 <- evalExpr expr2
    case add_op of
        Plus _ -> return $ TInt (n1 + n2)
        Minus _ -> return $ TInt (n1 - n2)

evalExpr (ERel _ expr1 rel_op expr2) = do
    TInt n1 <- evalExpr expr1
    TInt n2 <- evalExpr expr2
    case rel_op of
        LTH _ -> return $ TBool (n1 < n2)
        LE _ -> return $ TBool (n1 <= n2)
        GTH _ -> return $ TBool (n1 > n2)
        GE _ -> return $ TBool (n1 >= n2)
        EQU _ -> return $ TBool (n1 == n2)
        NE _ -> return $ TBool (n1 /= n2)

evalExpr (EAnd _ expr1 expr2) = do
    TBool b1 <- evalExpr expr1
    TBool b2 <- evalExpr expr2
    return $ TBool (b1 && b2)

evalExpr (EOr _ expr1 expr2) = do
    TBool b1 <- evalExpr expr1
    TBool b2 <- evalExpr expr2
    return $ TBool (b1 || b2)

evalExpr (EApp pos ident exprs) = do
    env <- ask
    let my_fun = Data.Map.lookup ident (envFun env)
    case my_fun of
        Nothing -> error "Unexpected failure! Function not found!"
        Just fun -> do
            let (my_args, my_env) = (funArgs fun, funEnv fun)
            new_env <- addArgs my_args exprs my_env
            (new_env, value) <- local (const new_env) (execBlock (funBlock fun))
            if value == TNone then 
                throwError ("No function return value! Error at: " ++ showPos pos)
            else 
                return value

execBlock :: Block -> ExecM (Env, Value)
execBlock (BlockR _ stmts) = do
    env <- ask
    (new_env, value) <- local (const env) (execStmts stmts)
    return (new_env, value)

execStmt :: Stmt -> ExecM (Env, Value)

execStmt (Ret _ expr) = do
    env <- ask
    value <- evalExpr expr
    return (env, value)

execStmt (Continue _) = do
    env <- ask
    let new_flags = (envFlags env) { loop_cont = True }
    return (env { envFlags = new_flags }, TNone)

execStmt (Break _) = do
    env <- ask
    let new_flags = (envFlags env) { loop_break = True }
    return (env {envFlags = new_flags }, TNone)

execStmt (Cond _ expr block) = do
    TBool cond <- evalExpr expr
    env <- ask
    if cond then do
        local (const env) (execBlock block)
    else
        return (env, TNone)

execStmt (CondElse _ expr block1 block2) = do
    TBool cond <- evalExpr expr
    env <- ask
    if cond then do
        local (const env) (execBlock block1)
    else do
        local (const env) (execBlock block2)

execStmt (FnDef _ ident args fun_type block) = do
    env <- ask
    let new_env = env { envFun = insert ident (Fun args new_env block) (envFun env) }
    return (new_env, TNone)

execStmt (While _ expr block) = do
    TBool cond <- evalExpr expr
    env <- ask
    if cond then do
        (new_env, val) <- local (const env) (execBlock block)
        if loop_break (envFlags new_env) then
            return (env, TNone)
        else do
            if val == TNone then
                local (const env) (execStmt (While Nothing expr block))
            else
                return (env, val)
    else
        return (env, TNone)

execStmt (Print _ []) = do
    env <- ask
    liftIO (putStrLn "")
    return (env, TNone)

execStmt (Ass _ ident expr) = do
    n <- evalExpr expr
    env <- ask
    let loc = varLoc ident (envVar env)
    modify (insert loc n)
    return (env, TNone)

execStmt (Print pos (expr : exprs)) = do
    n <- evalExpr expr
    liftIO (myShow n)
    execStmt (Print pos exprs)

execStmt (Decl _ var_type items) = do
    env <- addVars items
    return (env, TNone)

execStmt (SExpr _ expr) = do
    env <- ask
    evalExpr expr
    return (env, TNone)

execStmts :: [Stmt] -> ExecM (Env, Value)
execStmts [] = do
    env <- ask
    return (env, TNone)
execStmts (first_stmt : stmts) = do
    (env, val) <- execStmt first_stmt
    if loop_break (envFlags env) || loop_cont (envFlags env) then
        return (env, val)
    else do
        if val == TNone then
            local (const env) (execStmts stmts)
        else
            return (env, val)

execProgram :: Program -> IO ()
execProgram (ProgramR _ stmts) = do
    let resM = runExceptT (execStmts stmts)
    res <- runReaderT (evalStateT resM emptyStore) emptyEnv
    case res of
        Left err -> putStrLn $ err
        Right val -> putStrLn "Program finished with no errors :)"