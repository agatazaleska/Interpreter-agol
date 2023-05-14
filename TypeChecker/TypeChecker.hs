module TypeChecker.TypeChecker where

import Data.Map
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Except
import Agol.Abs
import System.Process (CreateProcess(env))
import Data.Text.Array (new, empty)
import TypeChecker.TypesAndUtils

checkArg :: Arg -> Expr -> Env -> TypeCheckM Env
checkArg (ArgR _ typ ident) expr fun_env = do
    env <- ask
    case expr of
        ERef _ ref_ident -> do
            case varType ref_ident (envVar fun_env) of
                Just vtype -> do
                    let arg_type = getVType typ
                    if arg_type /= vtype then
                        throwError ("Mismached argument type at " ++ showPos (exprPos expr))
                    else return env
                Nothing -> throwError ("Reference to undefined variable at " ++ showPos (exprPos expr))
        _ -> do
            let arg_type = getVType typ
            expr_type <- checkExpr expr        
            if arg_type /= expr_type then
                throwError ("Mismached argument type at " ++ showPos (exprPos expr))
            else return env

checkArgs :: [Arg] -> [Expr] -> Env -> BNFC'Position -> TypeCheckM Env
checkArgs (arg : args) (expr : exprs) env pos = do
    checkArg arg expr env
    checkArgs args exprs env pos

checkArgs [] [] env pos = ask
checkArgs _ _  env pos = throwError ("Mismached number of arguments at " ++ showPos pos)

addArg :: Arg -> Env -> TypeCheckM Env
addArg (ArgR _ typ ident) env = do
    let vtype = getVType typ
    let new_env = env { envVar = insert ident vtype (envVar env)}
    return new_env

addArgs :: [Arg] -> Env -> TypeCheckM Env
addArgs [] env = do
    return env
addArgs (arg:args) env = do
    new_env <- addArg arg env
    local (const new_env) (addArgs args new_env)

addVars :: [Item] -> Type -> TypeCheckM Env

addVars [] t = ask

addVars ((NoInit _ ident) : items) t = do
    env <- ask
    let vtype = getVType t
    let new_env = env { envVar = insert ident vtype (envVar env) }
    local (const new_env) (addVars items t)

addVars ((Init _ ident expr) : items) t = do
    env <- ask
    vtype <- checkExpr expr
    let expected_type = getVType t
    case expected_type of
        TList ltype -> do
            case ltype of
                TList _ -> throwError ("Only lists of simple types allowed! Error at " ++ showPos (exprPos expr))
                _ -> do
                    if vtype /= expected_type && vtype /= TList TNone then
                        throwError ("Mismached variable type at " ++ showPos (exprPos expr))
                    else do
                        let new_env = env { envVar = insert ident expected_type (envVar env) }
                        local (const new_env) (addVars items t)
        _ -> do
            if vtype /= expected_type then
                throwError ("Mismached variable type at " ++ showPos (exprPos expr))
            else do
                let new_env = env { envVar = insert ident expected_type (envVar env) }
                local (const new_env) (addVars items t)

checkExpr :: Expr -> TypeCheckM ValType

checkExpr (ELitInt _ n) = return TInt

checkExpr (ELitTrue _) = return TBool

checkExpr (ELitFalse _) = return TBool

checkExpr (EString _ str) = return TString

checkExpr (EList pos (expr : exprs)) = do
    vtype <- checkExpr expr
    mapM_ (\expr -> do
        expr_type <- checkExpr expr
        if expr_type /= vtype then
            throwError ("Mismached list element type at " ++ showPos pos)
        else return ()) exprs
    return (TList vtype)

checkExpr (EList pos []) = return (TList TNone)

checkExpr (Get pos ident expr) = do
    env <- ask
    case varType ident (envVar env) of
        Just vtype -> do
            case vtype of
                TList vtype -> do
                    expr_type <- checkExpr expr
                    if expr_type /= TInt then
                        throwError ("Attempted list indexing with non-integer at " ++ showPos pos)
                    else return vtype
                TString -> do
                    expr_type <- checkExpr expr
                    if expr_type /= TInt then
                        throwError ("Attempted list indexing with non-integer at " ++ showPos pos)
                    else return TString
                _ -> throwError ("Attempted list indexing on non-list at " ++ showPos pos)

        Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkExpr (EVar pos ident) = do
    env <- ask
    case varType ident (envVar env) of
        Just vtype -> do
            if vtype == TNone then
                throwError ("Attempted use of variable with no assigned value at " ++ showPos pos)
            else return vtype
        Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkExpr (ERef pos ident) = do
    env <- ask
    case varType ident (envVar env) of
        Just vtype -> return TInt
        Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkExpr (Neg pos expr) = do
    vtype <- checkExpr expr
    if vtype == TInt then return TInt
    else throwError ("Attempted arithmetic negation of non-integer at " ++ showPos pos)

checkExpr (Not pos expr) = do
    vtype <- checkExpr expr
    if vtype == TBool then return TBool
    else throwError ("Attempted logical negation of non-boolean at " ++ showPos pos)

checkExpr (EMul pos expr1 mul_op expr2) = do
    vtype1 <- checkExpr expr1
    vtype2 <- checkExpr expr2
    case vtype1 of
        TInt -> case vtype2 of
            TInt -> return TInt
            _ -> throwError ("Attempted arithmetic operation on non-integer at " ++ showPos pos)
        _ -> throwError ("Attempted arithmetic operation on non-integer at " ++ showPos pos)

checkExpr (EAdd pos expr1 add_op expr2) = do
    vtype1 <- checkExpr expr1
    vtype2 <- checkExpr expr2
    case vtype1 of
        TString -> case add_op of
            Plus _ -> do
                case vtype2 of
                    TString -> return TString
                    _ -> throwError ("Attempted string concatenation with non-string at " ++ showPos pos)
            _ -> throwError ("Attempted arithmetic operation on non-integer at " ++ showPos pos)
        TInt -> case vtype2 of
            TInt -> return TInt
            _ -> throwError ("Attempted arithmetic operation on non-integer at " ++ showPos pos)
        _ -> throwError ("Attempted arithmetic operation on non-integer at " ++ showPos pos)

checkExpr (ERel pos expr1 rel_op expr2) = do
    vtype1 <- checkExpr expr1
    vtype2 <- checkExpr expr2
    case rel_op of
        NE _ -> do
            if vtype1 /= vtype2 then
                throwError ("Attempted comparison between expressions of different types at " ++ showPos pos)
            else return TBool
        EQU _ -> do
            if vtype1 /= vtype2 then
                throwError ("Attempted comparison between expressions of different types at " ++ showPos pos)
            else return TBool
        _ -> case vtype1 of
            TInt -> case vtype2 of
                TInt -> return TBool
                _ -> throwError ("Attempted arithmetic comparison between non-integer types at " ++ showPos pos)
            _ -> throwError ("Attempted arithmetic comparison between non-integer types at " ++ showPos pos)

checkExpr (EAnd pos expr1 expr2) = do
    vtype1 <- checkExpr expr1
    vtype2 <- checkExpr expr2
    case vtype1 of
        TBool -> case vtype2 of
            TBool -> return TBool
            _ -> throwError ("Attempted logical operation on non-boolean at " ++ showPos pos)
        _ -> throwError ("Attempted logical operation on non-boolean at " ++ showPos pos)

checkExpr (EOr pos expr1 expr2) = do
    checkExpr (EAnd pos expr1 expr2)

checkExpr (EApp pos ident exprs) = do
    env <- ask
    let my_fun = Data.Map.lookup ident (envFun env)
    case my_fun of
        Nothing -> throwError ("Attempted use of undeclared function at " ++ showPos pos)
        Just fun -> do
            local (const env) (checkArgs (funArgs fun) exprs (funEnv fun) pos)
            return $ retType fun

checkStmt :: Stmt -> TypeCheckM Env

checkStmt (Ret pos expr) = do
    env <- ask
    (if inFunc $ envFlags env then (do
        etype <- checkExpr expr
        if etype /= funcType (envFlags env) then
            throwError ("Mismatched function return value type at " ++ showPos pos)
        else do
            let new_flags = (envFlags env) { ret = True }
            return env { envFlags = new_flags })
    else throwError ("Attempted return outside of a function at " ++ showPos pos))

checkStmt (Continue pos) = do
    env <- ask
    (if inLoop $ envFlags env then return env
        else throwError ("Attempted continue outside of a loop at " ++ showPos pos))

checkStmt (Break pos) = do
    env <- ask
    (if inLoop $ envFlags env then return env
        else throwError ("Attempted break outside of a loop at " ++ showPos pos))

checkStmt (SExpr _ expr) = do
    vtype <- checkExpr expr
    ask

checkStmt (Print pos (expr : exprs)) = do
    etype <- checkExpr expr
    checkStmt (Print pos exprs)

checkStmt (Print _ []) = ask

checkStmt (Cond pos expr block) = do
    etype <- checkExpr expr
    if etype /= TBool then
        throwError ("Attempted conditional statement with non-boolean condition at " ++ showPos pos)
    else do
        env <- ask
        new_env <- local (const env) (checkBlock block)
        return env {envFlags = envFlags new_env}

checkStmt (CondElse pos expr block1 block2) = do
    etype <- checkExpr expr
    if etype /= TBool then
        throwError ("Attempted conditional statement with non-boolean condition at " ++ showPos pos)
    else do
        env <- ask
        env1 <- local (const env) (checkBlock block1)
        env2 <- local (const env {envFlags = envFlags env1}) (checkBlock block2)
        return env {envFlags = envFlags env2}

checkStmt (While pos expr block) = do
    etype <- checkExpr expr
    if etype /= TBool then
        throwError ("While condition expression with non-boolean value at " ++ showPos pos)
    else do
        env <- ask
        let new_flags = (envFlags env) { inLoop = True }
        new_env <- local (const env {envFlags = new_flags}) (checkBlock block)
        return env {envFlags = (envFlags new_env) { inLoop = False }}

checkStmt (FnDef pos ident args fun_type block) = do
    env <- ask
    let new_flags = (envFlags env) { inFunc = True, funcType = getVType fun_type, ret = False }
    let new_fun = Fun { funArgs = args, retType = getVType fun_type, funEnv = env}
    let new_env = env { envFun = insert ident new_fun (envFun env) }
    let new_flags_env = new_env { envFlags = new_flags }

    loc_env <- addArgs args new_flags_env
    ret_env <- local (const loc_env) (checkBlock block)
    if ret $ envFlags ret_env then
        return new_env
    else throwError ("Missing return statement in function at " ++ showPos pos)

checkStmt (Ass pos ident expr) = do
    etype <- checkExpr expr
    env <- ask
    case Data.Map.lookup ident (envVar env) of
        Just vartype -> do
            if (vartype /= TNone) && (etype /= vartype) && (vartype /= TList TNone) then
                throwError ("Attempt of assigning a value to a variable of a different type at " ++ showPos pos)
                else do
                    let new_env = env { envVar = insert ident vartype (envVar env)}
                    return new_env
        Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkStmt (Decl _ var_type items) = do
    addVars items var_type

checkStmt (Append pos ident expr) = do
    env <- ask
    case varType ident (envVar env) of
        Just vtype -> do
            case vtype of
                TList list_vtype -> do
                    expr_type <- checkExpr expr
                    if expr_type /= list_vtype then
                        throwError ("Attempted list append with mismatched element type at " ++ showPos pos)
                    else return env
                _ -> throwError ("Attempted list append on non-list at " ++ showPos pos)
        Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkStmt (Put pos ident expr_i expr_val) = do
    env <- ask
    i_type <- checkExpr expr_i
    if i_type /= TInt then
        throwError ("Attempted put operation with non-integer index at " ++ showPos pos)
    else do
        case varType ident (envVar env) of
            Just vtype -> do
                case vtype of
                    TList list_vtype -> do
                        expr_type <- checkExpr expr_val
                        if expr_type /= list_vtype then
                            throwError ("Attempted list put with mismatched element type at " ++ showPos pos)
                        else return env
                    TString -> do
                        expr_type <- checkExpr expr_val
                        if expr_type /= TString then
                            throwError ("Attempted string put with non-string at " ++ showPos pos)
                        else return env
                    _ -> throwError ("Attempted put operation on type other than list and string at " ++ showPos pos)
            Nothing -> throwError ("Attempted use of undeclared variable at " ++ showPos pos)

checkStmts :: [Stmt] -> TypeCheckM Env
checkStmts [] = ask
checkStmts (first_stmt : stmts) = do
    env <- checkStmt first_stmt
    local (const env) (checkStmts stmts)

checkBlock :: Block -> TypeCheckM Env
checkBlock (BlockR _ stmts) = do
    env <- ask
    local (const env) (checkStmts stmts)

checkProgram :: Program -> Either String Env
checkProgram (ProgramR _ stmts) = do
    let env = emptyEnv
    let result = runReader (runExceptT (checkStmts stmts)) env
    case result of
        Left err -> Left err
        Right env -> Right env