module Interpreter.Interpreter where

import Data.Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except
import Agol.Abs
import Data.Text.Array (new)
import Interpreter.TypesAndUtils
import System.IO (stderr, hPutStrLn)

addArg :: Arg -> Expr -> Env -> ExecM Env
addArg (ArgR _ typ ident) expr fun_env = do
    case expr of
        ERef _ ref_ident -> do
            loc <- local (const fun_env) (getVarLoc ref_ident)
            let new_env = fun_env { envVar =
                insert ident loc (envVar fun_env) }
            return new_env
        _ -> do
            value <- evalExpr expr
            store <- get
            let new_loc = nextLoc store
            let new_env = fun_env { envVar = insert ident new_loc (envVar fun_env) }
            modify (insert new_loc value)
            return new_env

addArgs :: [Arg] -> [Expr] -> Env -> ExecM Env
addArgs [] [] fun_env = do
    return fun_env
addArgs (arg:args) (expr:exprs) fun_env = do
    new_env <- addArg arg expr fun_env
    addArgs args exprs new_env

addVars :: [Item] -> Type -> ExecM Env
addVars [] t = ask
addVars ((NoInit _ ident) : items) t = do
    env <- ask
    store <- get
    let new_env = env { envVar = insert ident (nextLoc store) (envVar env) }
    case t of
        Int _ -> modify (insert (nextLoc store) (VInt 0))
        Bool _ -> modify (insert (nextLoc store) (VBool False))
        Str _ -> modify (insert (nextLoc store) (VString ""))
        List _ _ -> modify (insert (nextLoc store) (VList []))
    local (const new_env) (addVars items t)

addVars ((Init _ ident expr) : items) t = do
    env <- ask
    store <- get
    value <- evalExpr expr
    let new_loc = nextLoc store
    let new_env = env { envVar = insert ident new_loc (envVar env) }
    modify (insert new_loc value)
    local (const new_env) (addVars items t)

evalExpr :: Expr -> ExecM Value

evalExpr (ELitInt _ n) = return (VInt n)

evalExpr (ELitTrue _) = return (VBool True)

evalExpr (ELitFalse _) = return (VBool False)

evalExpr (EString _ str) = return (VString str)

evalExpr (EList _ exprs) = do
    values <- mapM evalExpr exprs
    return (VList values)

evalExpr (Get pos ident expr) = do
    VInt i <- evalExpr expr
    object <- readVar ident
    case object of
        VList l -> do
            checkIndexValid pos i l
            return (l !! fromIntegral i)
        VString s -> do
            checkIndexValid pos i s
            return (VString [s !! fromIntegral i])

evalExpr (EVar _ ident) = do
    readVar ident

evalExpr (ERef _ ident) = do
    loc <- getVarLoc ident
    return (VInt loc)

evalExpr (Neg _ expr) = do
    VInt n <- evalExpr expr
    return (VInt (-n))

evalExpr (Not _ expr) = do
    VBool b <- evalExpr expr
    return (VBool (not b))

evalExpr (EMul pos expr1 mul_op expr2) = do
    VInt n1 <- evalExpr expr1
    VInt n2 <- evalExpr expr2
    case mul_op of
        Times _ ->  return $ VInt (n1 * n2)
        Div _ -> do
            if n2 == 0 then
                throwError ("Division by zero! Error at: " ++ showPos pos)
            else
                return $ VInt (n1 `div` n2)
        Mod _ -> return $ VInt (n1 `mod` n2)

evalExpr (EAdd _ expr1 add_op expr2) = do
    v1 <- evalExpr expr1
    case v1 of
        VString s1 -> do
            VString s2 <- evalExpr expr2
            case add_op of
                Plus _ -> return $ VString (s1 ++ s2)
        VInt n1 -> do
            VInt n2 <- evalExpr expr2
            case add_op of
                Plus _ -> return $ VInt (n1 + n2)
                Minus _ -> return $ VInt (n1 - n2)

evalExpr (ERel _ expr1 rel_op expr2) = do
    v1 <- evalExpr expr1
    case v1 of
        VString s1 -> do
            VString s2 <- evalExpr expr2
            case rel_op of
                EQU _ -> return $ VBool (s1 == s2)
                NE _ -> return $ VBool (s1 /= s2)
        VBool b1 -> do
            VBool b2 <- evalExpr expr2
            case rel_op of
                EQU _ -> return $ VBool (b1 == b2)
                NE _ -> return $ VBool (b1 /= b2)
        VList l1 -> do
            VList l2 <- evalExpr expr2
            case rel_op of
                EQU _ -> return $ VBool (l1 == l2)
                NE _ -> return $ VBool (l1 /= l2)
        VInt n1 -> do
            VInt n2 <- evalExpr expr2
            case rel_op of
                LTH _ -> return $ VBool (n1 < n2)
                LE _ -> return $ VBool (n1 <= n2)
                GTH _ -> return $ VBool (n1 > n2)
                GE _ -> return $ VBool (n1 >= n2)
                EQU _ -> return $ VBool (n1 == n2)
                NE _ -> return $ VBool (n1 /= n2)

evalExpr (EAnd _ expr1 expr2) = do
    VBool b1 <- evalExpr expr1
    VBool b2 <- evalExpr expr2
    return $ VBool (b1 && b2)

evalExpr (EOr _ expr1 expr2) = do
    VBool b1 <- evalExpr expr1
    VBool b2 <- evalExpr expr2
    return $ VBool (b1 || b2)

evalExpr (EApp pos ident exprs) = do
    env <- ask
    let my_fun = Data.Map.lookup ident (envFun env)
    case my_fun of
        Nothing -> throwError "Unexpected failure! Function not found!"
        Just fun -> do
            let (my_args, my_env) = (funArgs fun, funEnv fun)
            new_env <- local (const env)  (addArgs my_args exprs my_env)
            (new_env, value) <- local (const new_env) (execBlock (funBlock fun))
            if value == VNone then
                throwError ("No function return value! Error at: " ++ showPos pos)
            else
                return value

execStmt :: Stmt -> ExecM (Env, Value)

execStmt (Ret _ expr) = do
    env <- ask
    value <- evalExpr expr
    return (env, value)

execStmt (Continue _) = do
    env <- ask
    let new_flags = (envFlags env) { loop_cont = True }
    return (env { envFlags = new_flags }, VNone)

execStmt (Break _) = do
    env <- ask
    let new_flags = (envFlags env) { loop_break = True }
    return (env {envFlags = new_flags }, VNone)

execStmt (SExpr _ expr) = do
    env <- ask
    evalExpr expr
    return (env, VNone)

execStmt (Print pos (expr : exprs)) = do
    n <- evalExpr expr
    liftIO (myShow n)
    execStmt (Print pos exprs)

execStmt (Print _ []) = do
    env <- ask
    liftIO (putStrLn "")
    return (env, VNone)

execStmt (Cond _ expr block) = do
    VBool cond <- evalExpr expr
    env <- ask
    if cond then do
        (new_env, val) <- local (const env) (execBlock block)
        return (env {envFlags = envFlags new_env}, val)
    else
        return (env, VNone)

execStmt (CondElse _ expr block1 block2) = do
    VBool cond <- evalExpr expr
    env <- ask
    if cond then do
        (new_env, val) <- local (const env) (execBlock block1)
        return (env {envFlags = envFlags new_env}, val)
    else do
        (new_env, val) <- local (const env) (execBlock block2)
        return (env {envFlags = envFlags new_env}, val)

execStmt (While pos expr block) = do
    env <- ask
    VBool cond <- evalExpr expr
    if cond then do
        (new_env, val) <- local (const env) (execBlock block)
        if loop_break (envFlags new_env) then
            return (env, VNone)
        else do
            if val == VNone then do
                (new_env, new_val) <- local (const env) (execStmt (While pos expr block))
                return (env {envFlags = envFlags new_env}, new_val)
            else
                return (env, val)
    else
        return (env, VNone)

execStmt (FnDef _ ident args fun_type block) = do
    env <- ask
    let new_env = env { envFun = insert ident (Fun args new_env block) (envFun env) }
    return (new_env, VNone)

execStmt (Ass _ ident expr) = do
    env <- ask
    n <- evalExpr expr
    loc <- getVarLoc ident
    modify (insert loc n)
    return (env, VNone)

execStmt (Decl _ var_type items) = do
    env <- addVars items var_type
    return (env, VNone)

execStmt (Append _ ident expr) = do
    env <- ask
    val <- evalExpr expr
    loc <- getVarLoc ident
    VList l <- readVar ident
    modify (insert loc (VList (l ++ [val])))
    return (env, VNone)

execStmt (Put pos ident expr_i expr_val) = do
    env <- ask
    VInt i <- evalExpr expr_i
    loc <- getVarLoc ident
    object <- readVar ident
    case object of
        VList l -> do
            checkIndexValid pos i l
            val <- evalExpr expr_val
            let new_list = Prelude.take (fromIntegral i) l ++ [val] ++ Prelude.drop (fromIntegral (i + 1)) l
            modify (insert loc (VList new_list))
            return (env, VNone)
        VString s -> do
            checkIndexValid pos i s
            VString ch <- evalExpr expr_val
            if length ch /= 1 then
                throwError ("Only one-character string allowed as put function argument! Error at: " ++ showPos pos)
            else do
                let new_string = Prelude.take (fromIntegral i) s ++ ch ++ Prelude.drop (fromIntegral (i + 1)) s
                modify (insert loc (VString new_string))
                return (env, VNone)

execStmts :: [Stmt] -> ExecM (Env, Value)
execStmts [] = do
    env <- ask
    return (env, VNone)
execStmts (first_stmt : stmts) = do
    (env, val) <- execStmt first_stmt
    if loop_break (envFlags env) || loop_cont (envFlags env) then
        return (env, val)
    else do
        if val == VNone then
            local (const env) (execStmts stmts)
        else
            return (env, val)

execBlock :: Block -> ExecM (Env, Value)
execBlock (BlockR _ stmts) = do
    env <- ask
    (new_env, value) <- local (const env) (execStmts stmts)
    return (new_env, value)

execProgram :: Program -> IO ()
execProgram (ProgramR _ stmts) = do
    let resM = runExceptT (execStmts stmts)
    res <- runReaderT (evalStateT resM emptyStore) emptyEnv
    case res of
        Left err -> hPutStrLn stderr err
        Right val -> return ()