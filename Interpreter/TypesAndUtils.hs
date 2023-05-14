module Interpreter.TypesAndUtils where

import Data.Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except
import Agol.Abs
import Data.Text.Array (new)

data Value = VInt Integer | VBool Bool | VString String | VList [Value] | VNone
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

showPos :: BNFC'Position -> String
showPos Nothing = "Unknown position."
showPos (Just (line, col)) = "line " ++ show line ++ ", column " ++ show col ++ "."

myShow :: Value -> IO()
myShow (VInt x) = putStr (show x)
myShow (VBool x) = putStr (show x)
myShow (VString x) = putStr x
myShow (VList (x : xs)) = do
    putStr "["
    myShowList (VList (x : xs))
    putStr "]"

myShowList :: Value -> IO()
myShowList (VList []) = return ()
myShowList (VList [x]) = myShow x
myShowList (VList (x : xs)) = do
    myShow x
    putStr ", "
    myShowList (VList xs)

checkIndexValid :: BNFC'Position -> Integer -> [a] -> ExecM ()
checkIndexValid pos i l = do
    if fromIntegral i >= length l then
        throwError ("Index out of bounds! Error at: " ++ showPos pos)
    else if i < 0 then
        throwError ("Negative index! Error at: " ++ showPos pos)
    else return ()

nextLoc :: Store -> Loc
nextLoc store = do
    case Data.Map.lookupMax store of
        Just (value, _) -> value + 1
        Nothing -> 0

getVarLoc :: Ident -> ExecM Loc
getVarLoc ident = do
    env <- ask
    case Data.Map.lookup ident (envVar env) of
        Just value -> return value
        Nothing -> throwError "Unexpected failure! variable not found!"

valueAtLoc :: Loc -> ExecM Value
valueAtLoc loc = do
    store <- get
    case Data.Map.lookup loc store of
        Just value -> return value
        Nothing -> throwError "Unexpected failure! location not found!"

readVar :: Ident -> ExecM Value
readVar ident = do
    loc <- getVarLoc ident
    valueAtLoc loc