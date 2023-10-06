### Agata Załęska - description of the Task - Interpreter

#### Language Description 

The language I chose for the this task is quite standard, without fancy syntax elements or conceptually challenging features.
I drew some inspiration from the Latte language, but Agol differs from it in several extensions and syntax.
The language is statically typed.

###### Program Structure

A program consists of a sequence of statements separated by semicolons.
Statements include "while," "if," "else," "break," "continue," "return," as well as assignments,
variable and function declarations, function calls, and calls to built-in functions in the language.

###### Types 

The available standard types are `int`, `bool`, `string`, and simple non-nested lists.

###### Arithmetic

Variables of type `int` can be compared using `(<, <=, ==, >, >=, !=)` and subjected to
basic arithmetic operations `(+, -, *, /, ^)`. You can also compare and concatenate strings.
Variables of type `boolean` and lists can also be compared.

###### Function Parameters

Function parameters can be passed either by value or by reference.

###### Other Language Features

There is a built-in function `print()`, which takes variables and literals of any type as arguments.
There is also a `.get(i)` function, which returns the i-th element of a list or string, and an `.append(expr)`
function that adds a given element to the end of a list. Additionally, there is a `.put(i, expr)`
function for strings and lists that changes the element at index `i` to `expr`.

All errors resulting from operations on incorrect types are checked statically before the program is executed.

#### Interpreter

In the "Interpreter" folder, you will find files responsible for implementing the interpreter.
The "TypesAndUtils.hs" file contains declared types used in the implementation and simple helper functions.

The solution is based on a monad with the composed type
`type ExecM a = ExceptT String (StateT Store (ReaderT Env IO)) a`, where `Env` is the environment,
and `Store` is the state. This monad allows for throwing runtime errors, storing the constant environment value
and variable state value, and printing information to the output.

###### Handling Environment and State

The environment is defined by the object of type `data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }`, where

```
data Flags = Flags { loop_break :: Bool, loop_cont :: Bool }
type EnvVar = Map Ident Loc
type EnvFun = Map Ident Fun

type Loc = Integer
data Fun = Fun { funArgs :: [Arg], funEnv :: Env, funBlock :: Block }
```

The environment stores information about declared variables and their locations,
as well as declared functions. It also stores flags used during program interpretation to handle loop interruption operations.

The state is defined by the object of type `type Store = Map Loc Value`, where

`data Value = VInt Integer | VBool Bool | VString String | VList [Value] | VNone`

The state stores information about the values assigned to specific locations. 
If no value is assigned to a variable at declaration, each type has a default value assigned.

###### Main Functions Responsible for Proper Program Execution

- `execProgram :: Program -> IO ()` - This function starts the entire program (calls `execStmts`). If it finishes with a runtime error, it prints the error message to the standard error output.
- `execStmts :: [Stmt] -> ExecM (Env, Value)` - This function executes a list of statements. It handles `break`, `continue`, and `return` statements - after encountering one of these, it does not execute subsequent statements.
- `execBlock :: Block -> ExecM (Env, Value)`-  This function executes a block of statements, essentially just executing `execStmts` and returning the modified environment.
- `execStmt :: Stmt -> ExecM (Env, Value)` - This function uses the `ExecM` monad to execute a given statement. It returns a pair of `(Env, Value)` because some statements modify the environment (e.g., variable or function declarations). For compound statements (e.g., `while`) in which a return statement appears, or the return statement itself, it returns a specific value (the result of a function). Other statements return `VNone`.
- `evalExpr :: Expr -> ExecM Value` - This function is responsible for evaluating expressions. Since an expression results in a value, it returns a `Value`.

#### TypeChecker

In the "TypeChecker" folder, you will find files responsible for implementing static type checking and error detection (e.g., using undeclared variables). The "TypesAndUtils.hs" file contains declared types used in the implementation and simple helper functions.

The solution is based on a monad with the composed type `type TypeCheckM a = ExceptT String (Reader Env) a`. In this case, all we need is error handling and an immutable environment.

###### Environment Handling

The environment is defined by the object of type `data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }`, where

```
type EnvVar = Map Ident ValType
type EnvFun = Map Ident Fun
data Flags = Flags { inFunc :: Bool, funcType :: ValType, ret :: Bool, inLoop :: Bool }

data Fun = Fun { funArgs :: [Arg], retType :: ValType, funEnv :: Env }
data ValType = TInt | TString | TBool | TList ValType | TNone
```

It stores flags:

- `inFunc`, to handle the error of `return` statements outside of functions.
- `funcType`, to handle the error of an invalid return type of a function.
- `ret`, to handle the error of a missing `return` statement in a function body
- `inLoop`, to handle `break` or `continue` statements outside of loops.

It also stores the types of declared variables and functions.

###### Main Functions Responsible for Proper Program Execution

- `checkProgram :: Program -> Either String Env` -  This function checks the entire program (it calls `checkStmts`). Its result is either an error message or the environment (though its value is not used later).
- `checkStmts :: [Stmt] -> TypeCheckM Env` - This function checks a list of statements. It sequentially checks all statements.
- `checkBlock :: Block -> TypeCheckM Env` - This function checks a block of statements. It simply uses `checkStmts`.
- `checkStmt :: Stmt -> TypeCheckM Env` - This function checks a single statement. Similar to the interpreter, it returns the environment because some statements can modify it.
- `checkExpr :: Expr -> TypeCheckM ValType` - Similar to the interpreter, this function checks the correctness of an expression and returns its type.



#### Running the Program

To build the program, use the `make` command.

You can run it with the command `./interpreter` or `./interpreter [filename]`.

Additionally, in the main folder, there are scripts `run_good.sh` and `run_bad.sh` that run all files in the "good" or "bad" folder.

