### Agata Załęska - opis rozwiązania zadania zaliczeniowego - Interpreter

#### Opis języka 

Język, który wybrałam do zadania zaliczeniowego Interpreter jest raczej standardowy, nie ma w nim wymyślnych elementów składniowych ani trudnych koncepcyjnie funkcjonalności. Lekko inspirowałam się językiem Latte, jednak mój język różni się od niego kilkoma rozszerzeniami oraz składnią. Język jest statycznie typowany.

###### Struktura programu

Program jest ciągiem instrukcji oddzielonych średnikami. Instrukcje to między innymi ``while, if, else, break, continue, return``,  a także przypisania, deklaracje zmiennych i funkcji, wywołanie funkcji zadeklarowanych oraz wywołanie dodatkowych funkcji wbudowanych w język.

###### Typy 

Dostępne są standardowe typy:  `int, bool, string`, oraz listy prostego typu (bez zagnieżdżania).

###### Arytmetyka

Zmienne typu int można porównywać `(<, <=, ==, >, >=, !=)`, oraz wykonywać na nich podstawowe operacje arytmetyczne `(+, -, *, /, ^)`. Stringi także można porównywać oraz dodawać. Zmienne typu `boolean` i listy także można porównywać.

###### Parametry funkcji

Parametry do funkcji mogą być przekazywanie przez wartość i zmienną.

###### Inne cechy języka

Dostępna jest funkcja wieloargumentowa `print()`, która jako argumenty przyjmuje zmienne i literały dowolnego typu.
Jest także funkcja `` .get(i)``, która zwraca i-ty element listy lub stringa, oraz funkcja `` .append(expr)``, która dodaje na koniec listy dany element, a także funkcja ``.put(i, expr)`` działająca dla stringa i listy zmieniająca element pod indexem ``i`` na ``expr``.

Wszelkie błędy wynikające z operowania na niepoprawnych typach są sprawdzane statycznie, przed rozpoczęciem wykonywania programu.



#### Interpreter

W folderze `Interpreter` znajdują się pliki odpowiedzialne za implementację interpretera. W pliku `TypesAndUtils.hs` znajdują się zadeklarowane typy użyte w implementacji oraz proste funkcje pomocnicze.

Rozwiązanie opiera się na monadzie złożonej typu `type ExecM a = ExceptT String (StateT Store (ReaderT Env IO)) a`, gdzie `Env` to środowisko, a `Store` stan. Umożliwia ona rzucanie błędów wykonania, przechowywanie stałej wartości środowiska i zmiennej wartości stanu, a także wypisywanie informacji na wyjście.

###### Obsługa środowiska i stanu

Środowisko jest zadane obiektem typu `data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }`, gdzie

```
data Flags = Flags { loop_break :: Bool, loop_cont :: Bool }
type EnvVar = Map Ident Loc
type EnvFun = Map Ident Fun

type Loc = Integer
data Fun = Fun { funArgs :: [Arg], funEnv :: Env, funBlock :: Block }
```

Środowisko przechowuje informacje o zadeklarowanych zmiennych i ich lokacjach, a także o zadeklarowanych funkcjach. Dodatkowo przechowuje flagi używane w trakcie interpretacji programu w celu obsługi operacji przerywania pętli.

Stan zadany jest obiektem typu `type Store = Map Loc Value`, gdzie

`data Value = VInt Integer | VBool Bool | VString String | VList [Value] | VNone`

Stan przechowuje informacje o wartościach przypisanych do danych lokacji.
W przypadku braku przypisania wartości zmiennej przy deklaracji, każdy typ ma przypisaną domyślną wartość.

###### Główne funkcje odpowiadające za prawidłowe wykonanie programu

- `execProgram :: Program -> IO ()` - Funkcja uruchamiające cały program (wywołuje `execStmts`). Jeżeli zakończy się błędem wykonania, wypisuje treść błędu na standardowe wyjście diagnostyczne.
- `execStmts :: [Stmt] -> ExecM (Env, Value)` - Funkcja wykonująca listę instrukcji. Zajmuje się wychwyceniem `break`, `continue` i `return` - po otrzymaniu jednej z tych instrukcji nie wykonuje kolejnych.
- `execBlock :: Block -> ExecM (Env, Value)`- Funkcja wykonująca blok instrukcji - w zasadzie po prostu wykonuje `execStmts` i zwraca zmienione środowisko.
- `execStmt :: Stmt -> ExecM (Env, Value)` - Funkcja korzysta z monady `ExecM` i wykonuje daną instrukcję. Wartością zwracaną jest para `(Env, Value)`, ponieważ niektóre instrukcje modyfikują środowisko (na przykład deklaracja zmiennej lub funkcji). 
  `Value` używane do obsługi zwracania wartości z funkcji. Instrukcje złożone (na przykład `while`), w których pojawi się return oraz sam return zwracają określoną wartość (wynik funkcji). Pozostałe instrukcje zwracają `VNone`.
- `evalExpr :: Expr -> ExecM Value` - Funkcja odpowiedzialna za wyliczania wyrażeń. Ponieważ wyrażenie kończy się wynikiem, jej wartością zwracaną jest `Value`. 



#### TypeChecker

W folderze `TypeChecker` znajdują się pliki odpowiedzialne za implementację statycznego sprawdzania typów oraz wykrywania błędów (na przykład użycie niezadeklarowanej zmiennej). W pliku `TypesAndUtils.hs` znajdują się zadeklarowane typy użyte w implementacji oraz proste funkcje pomocnicze.

Rozwiązanie opiera się na monadzie złożonej typu `type TypeCheckM a = ExceptT String (Reader Env) a`. W tym przypadku wystarczy obsługa błędów i niezmiennego środowiska.

###### Obsługa środowiska

Środowisko jest zadane obiektem typu `data Env = Env { envVar :: EnvVar, envFun :: EnvFun, envFlags :: Flags }`, gdzie

```
type EnvVar = Map Ident ValType
type EnvFun = Map Ident Fun
data Flags = Flags { inFunc :: Bool, funcType :: ValType, ret :: Bool, inLoop :: Bool }

data Fun = Fun { funArgs :: [Arg], retType :: ValType, funEnv :: Env }
data ValType = TInt | TString | TBool | TList ValType | TNone
```

Są w nim przechowywane flagi:

- `inFunc`, do obsługi błędu w postaci instrukcji `return` poza funkcją
- `funcType`, do obsługi błędu w postaci nieprawidłowego typu wartości zwróconej z funkcji.
- `ret`, do obsługi błędu w postaci braku jakiejkolwiek instrukcji `return` w ciele funkcji.
- `inLoop`, do obsługi błędu w postaci instrukcji `break` lub `continue` poza pętlą.

A także typy zadeklarowanych zmiennych oraz funkcje.

###### Główne funkcje odpowiadające za prawidłowe wykonanie programu

- `checkProgram :: Program -> Either String Env` - Funkcja sprawdzająca cały program (wywołuje `checkStmts`). Jej wynikiem jest komunikat błędu lub środowisko (jego wartość nie jest później używana).
- `checkStmts :: [Stmt] -> TypeCheckM Env` - Funkcja sprawdzająca listę instrukcji. Po kolei sprawdza wszystkie instrukcje.
- `checkBlock :: Block -> TypeCheckM Env` - Funkcja sprawdzająca blok instrukcji. Po prostu korzysta z `checkStmts`.
- `checkStmt :: Stmt -> TypeCheckM Env` - Funkcja sprawdzające pojedynczą instrukcję. Podobnie jak w interpreterze, zwracam środowisko, ponieważ niektóre instrukcje mogą je zmienić. 
- `checkExpr :: Expr -> TypeCheckM ValType` - Analogicznie jak w interpreterze. Funkcja sprawdza poprawność wyrażenia i zwraca jego typ.



#### Uruchomienie programu

Program buduje się poleceniem make.

Kolejno uruchamia się go poleceniem `./interpreter` lub `./interpreter [nazwa pliku]`.

Dodatkowo w folderze głównym są skrypty `run_good.sh` oraz `run_bad.sh` uruchamiające wszystkie pliki z folderu `good` lub `bad`.



#### Czego się nie udało

Niestety nie zdążyłam zaimplementować krotek z przypisaniem.

###### Zaktualizowana tabelka cech

```
  Na 15 punktów
  + 01 (trzy typy)
  + 02 (literały, arytmetyka, porównania)
  + 03 (zmienne, przypisanie)
  + 04 (print)
  + 05 (while, if)
  + 06 (funkcje lub procedury, rekurencja)
  + 07 (przez zmienną / przez wartość / in/out)
  - 08 (zmienne read-only i pętla for)
  Na 20 punktów
  + 09 (przesłanianie i statyczne wiązanie)
  + 10 (obsługa błędów wykonania)
  + 11 (funkcje zwracające wartość)
  Na 30 punktów
  + 12 (4) (statyczne typowanie)
  + 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  + 14 (1) (rekordy/listy/tablice)
  - 15 (2) (krotki z przypisaniem)
  + 16 (1) (break, continue)
  - 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  - 18 (3) (generatory)

Razem: 28p
```

