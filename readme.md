# Compilateur et machine virtuelle CLISP

Réalisé en M1 Génie Logiciel par Clément Mazet

Ce projet contient un compilateur CLISP vers Assembleur et une machine virtuelle permettant d'exécuter le code assembleur généré

## Démarrage express
- Se positionner dans le répertoire `test`
- Lancer clisp
- `(load "tests.lisp")` charge tout le code nécessaire aux tests unitaires
- `(run-all-tests)` exécute tous les tests unitaires
- `(run-comp-tests)` exécute tous les tests unitaires concernant la compilation de LISP vers ASM
- `(run-vm-tests)` exécute tous les tests unitaires concernant les instructions de la VM