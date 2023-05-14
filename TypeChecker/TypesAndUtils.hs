module TypeChecker.TypesAndUtils where
    
import Data.Map
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Except
import Agol.Abs
import Data.Text.Array (new, empty)

type EnvVar = Map Ident ValType
type EnvFun = Map Ident Fun

data ValType = TInt | TString | TBool | TList ValType | TNone
    deriving (Eq, Show)

data Flags = Flags { inFunc :: Bool, funcType :: ValType, ret :: Bool, inLoop :: Bool }
data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }
data Fun = Fun { funArgs :: [Arg], retType :: ValType, funEnv :: Env }

type TypeCheckM a = ExceptT String (Reader Env) a

emptyFlags :: Flags
emptyFlags = Flags { inFunc = False, funcType = TNone, ret = False, inLoop = False }

emptyEnv :: Env
emptyEnv = Env {envVar = Data.Map.empty, envFun = Data.Map.empty, envFlags = emptyFlags}

varType :: Ident -> EnvVar -> Maybe ValType
varType = Data.Map.lookup

showPos :: BNFC'Position -> String
showPos Nothing = "Unknown position."
showPos (Just (line, col)) = "line " ++ show line ++ ", column " ++ show col ++ "."

getVType :: Type -> ValType
getVType (Int _) = TInt
getVType (Str _) = TString
getVType (Bool _) = TBool
getVType (List _ t) = TList (getVType t)

exprPos :: Expr -> BNFC'Position
exprPos (EVar pos _) = pos
exprPos (ELitInt pos _) = pos
exprPos (ELitTrue pos) = pos
exprPos (ELitFalse pos) = pos
exprPos (EApp pos _ _) = pos
exprPos (EString pos _) = pos
exprPos (Neg pos _) = pos
exprPos (Not pos _) = pos
exprPos (EMul pos _ _ _) = pos
exprPos (EAdd pos _ _ _) = pos
exprPos (ERel pos _ _ _) = pos
exprPos (EAnd pos _ _) = pos
exprPos (EOr pos _ _) = pos
exprPos (ERef pos _) = pos
exprPos (EList pos _) = pos
exprPos (Get pos _ _) = pos