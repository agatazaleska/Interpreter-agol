### Agata Załęska - opis rozwiązania zadania zaliczeniowego - Interpreter

###### TypeChecker

W folderze TypeChecker znajdują się pliki odpowiedzialne za implementację type checkera.



###### Interpreter

W folderze `Interpreter` znajdują się pliki odpowiedzialne za implementację interpretera. W pliku `TypesAndUtils.hs` znajdują się zadeklarowane typy użyte w implementacji oraz proste funkcje pomocnicze.

Rozwiązanie opiera się na monadzie złożonej typu `type ExecM a = ExceptT String (StateT Store (ReaderT Env IO)) a`, gdzie `Env` to środowisko, a `Store` stan. Umożliwia ona rzucanie błędów wykonania, przechowywanie stałej wartości środowiska i zmiennej wartości stanu, a także wypisywanie informacji na wyjście.

Główne funkcje odpowiadające za prawidłowe wykonanie programu:

- a
- b
- `execStmt :: Stmt -> ExecM (Env, Value)` - Funkcja korzysta z monady `ExecM` i wykonuje daną instrukcję. Wartością zwracaną jest para `(Env, Value)`, ponieważ niektóre instrukcje modyfikują środowisko (na przykład deklaracja zmiennej lub funkcji). 
  `Value` używane do obsługi zwracania wartości z funkcji. Instrukcje złożone (na przykład `while`), w których pojawi się return oraz sam return zwracają określoną wartość (wynik funkcji). Pozostałe instrukcje zwracają `VNone`.



###### Czego się nie udało



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
  + 14 ([1]/2) (rekordy/listy/tablice/tablice wielowymiarowe)
  - 15 (2) (krotki z przypisaniem)
  + 16 (1) (break, continue)
  - 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  - 18 (3) (generatory)

Razem: 28p
```

