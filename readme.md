# Compilateur et machine virtuelle CLISP

Réalisé en M1 Génie Logiciel par Clément Mazet

Ce projet contient un compilateur CLISP vers Assembleur et une machine virtuelle permettant d'exécuter le code assembleur généré

Il est possible de compiler le compilateur mais pas le chargeur

## Tests express
- Se positionner dans le répertoire `test`
- Lancer clisp
- `(load "tests.lisp")` charge tout le code nécessaire
- `(run-all-tests)` exécute 107 tests unitaires
- `(run-comp-tests)` exécute 53 tests unitaires concernant la compilation de LISP vers ASM
- `(run-vm-tests)` exécute 54 tests unitaires concernant les instructions de la VM

## Tester soi-même
Chargement du code nécessaire(compilateur et VM)
- `(load "tests.lisp")`

Création d'une VM
- `(make-vm affichage-initial?->T/NIL nomvm taille)`

Par exemple `(make-vm T 'vmtest 500)`

Compilation puis chargement d'un code lisp vers asm dans la VM
- `(vm-load (compilation code-lisp '() '()) affichage-chargement?->T/NIL nomvm)`

Par exemple `(vm-load (compilation '((defun fibo (n) (if (= 0 n) 0 (if (= 1 n) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))(fibo 10)) '() '()) T 'vmtest)`

Exécution du code présent en mémoire
- `(vm-exec affichage-execution->T/NIL executer-step-by-step->T/NIL 'nomvm)`

Par exemple `(vm-exec T NIL 'vmtest)`

Compilation d'un fichier lisp vers un fichier asm
- ` (compile-fichier "file-name-in.lisp" "file-name-out.asm")`

Par exemple `(compile-fichier "factorial.lisp" "factorial.asm")`

Chargement d'un fichier asm dans la VM
- `(vm-load-file "file-name-out.asm" affichage-chargement?->T/NIL nomvm)`

Par exemple `(vm-load-file "factorial.asm" T 'vmtest)`

## Exemples complets
Fibo de 10

`(make-vm)(vm-load (compilation '((defun fibo (n) (if (= 0 n) 0 (if (= 1 n) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))(fibo 10)) '() '()))(vm-exec)(vm-print)`
ou

`(make-vm)(compile-fichier "fibonacci.lisp" "fibonacci.asm")(vm-load-file "fibonacci.asm")(vm-exec)(vm-print)`

Fact de 6

`(make-vm)(vm-load (compilation '((defun fact (n) (if (= n 1) 1 (* n (fact (- n 1))))) (fact 6)) '() '()))(vm-exec)(vm-print)`
ou

`(make-vm)(compile-fichier "factorial.lisp" "factorial.asm")(vm-load-file "factorial.asm")(vm-exec)(vm-print)`

## Expressions lisp prises en charges par le compilateur
- Littéraux
- Backquote
- And
- Or
- If
- Cond
- Loop while do
- Setq
- Defun
- Appels de fonctions
- Progn
- Opérateurs (+ - * /)
- Comparateurs (< = > <= =>)
- Let 
- Let*