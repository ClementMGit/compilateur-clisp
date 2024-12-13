(require "compiler.lisp")
(require "compiler/../vm/vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()
    (test-if);2 Tests
    (test-comparators);13 Tests
    (test-arithmetic-operators);5 Tests
    (test-progn);1 Test
    (test-cond);2 Tests
    (test-and-or);12 Tests
    (test-fonctions);5 Tests
    (test-let);2 Tests
    (test-setq);1 Test
    (test-while);2 Tests

    (format t "~%Nombre total de tests réussis : ~D/45~%" test-count)
)

(defun compTest (code expected-value comparator)
  "Fonction générique d'un cas de test unitaire"
  (format t "~%Test de compilation de '~A'~%" code)
  (make-vm NIL 'vm 500);VM sans affichage mémoire initial
  (vm-load (compilation code '() '()) T)
  (vm-exec)
  (if (funcall comparator (get 'vm :R0) expected-value)
    (progn
      (setq test-count (+ test-count 1))
      (format t "~%Test de compilation de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
    (format t "~%Test de compilation de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
)
(defun test-and-or ()
  (compTest '(and (= 1 1) (= 2 2)) 1 #'=)
  (compTest '(and (= 9 1) (= 2 2)) 0 #'=)
  (compTest '(and (= 1 1) (= 3 2)) 0 #'=)
  (compTest '(and (= 0 1) (= 3 2)) 0 #'=)
  (compTest '(and (and (= 1 1) (= 1 1)) (= 2 2)) 1 #'=)

  (compTest '(or (= 9 1) (= 2 2)) 1 #'=)
  (compTest '(or (= 1 1) (= 4 2)) 1 #'=)
  (compTest '(or (= 1 1) (= 2 2)) 1 #'=)
  (compTest '(or (= 0 1) (= 1 2)) 0 #'=)
  (compTest '(or (or (= 2 1) (= 1 1)) (= 1 2)) 1 #'=)

  (compTest '(and (or (= 2 1) (= 1 1)) (= 1 1)) 1 #'=)
  (compTest '(and (or (= 2 1) (= 1 1)) (= 2 1)) 0 #'=)
)

(defun test-arithmetic-operators ()
  (compTest '(+ 1 1) 2 #'=)
  (compTest '(- 3 1) 2 #'=)
  (compTest '(* 1 2) 2 #'=)
  (compTest '(/ 4 2) 2 #'=)
  (compTest '(/ (+ (+ 5 5) (/ 4 2)) 6) 2 #'=)    
)
(defun test-if ()
  (compTest '(if 1 100 2) 100 #'=)
  (compTest '(if nil 100 2) 2 #'=)
)
(defun test-cond ()
  (compTest '(cond ((< 10 0) (+ 1 1)) ((> 10 0) (+ 0 0))) 0 #'=)
  (compTest '(cond ((< -10 0) (+ 1 1)) ((> 10 0) (+ 0 0))) 2 #'=)
)
(defun test-comparators ()
  (compTest '(= 1 1) 1 #'=)
  (compTest '(= 0 1) 0 #'=)
  (compTest '(< 1 2) 1 #'=)
  (compTest '(< 2 1) 0 #'=)
  (compTest '(> 2 1) 1 #'=)
  (compTest '(> 1 2) 0 #'=)
  (compTest '(>= 2 2) 1 #'=)
  (compTest '(>= 2 1) 1 #'=)
  (compTest '(>= 1 2) 0 #'=)
  (compTest '(<= 2 2) 1 #'=)
  (compTest '(<= 1 2) 1 #'=)
  (compTest '(<= 2 1) 0 #'=)
  (compTest '(= (+ 1 1) (+ 1 1)) 1 #'=)
)
(defun test-while ()
  ;Boucle while incrémentation d'un counter jusqua 10
  (compTest '(progn (defun add (x) (let ((counter 0)) (progn (loop while (< counter 10) do  (setq counter (+ counter 1))  ) (= counter 10)))) (add 90) ) 1 #'=)
  (compTest '(progn (defun add (x) (let ((counter 0)) (progn (loop while (> counter 10) do  (setq counter (+ counter 1))) (= counter 0)))) (add 90) ) 1 #'=)
)
(defun test-progn ()
  (compTest '(progn (+ 1 1) (+ 2 2)) 4 #'=)
)
(defun test-let ()
  ;Simple
  (compTest '(progn (defun add (x) (let ((a 10)) (+ a x))) (add 90)) 100 #'=)
  ;Nested, équivalent à un let*
  (compTest '(progn (defun add (x) (let ((a 5)) (let ((b 3)) (+ (+ a b) x))))(add 10)) 18 #'=)
)
(defun test-setq ()
  ;Test de modification d'une variable locale UNIQUEMENT AU SEIN D'UN LET D'UNE FONCTION
  (compTest '(progn (defun add (x) (let ((a 1)) (progn (setq a 55) (+ a x)))) (add 45)) 100 #'=)
)
(defun test-fonctions ()
  (compTest '(progn (defun add (x y) (+ x y))(add 1 2)) 3 #'=)
  (compTest '(progn (defun add (x y) (+ x y))(add 8 2)) 10 #'=)
  (compTest '(progn (defun fact (n) (if (= n 1) 1 (* n (fact (- n 1))))) (fact 6)) 720 #'=)
  (compTest '(progn (defun add (x y) (+ x y))(add (add 6 4) 3)) 13 #'=)
  (compTest '(progn (defun fibo (n)
    (if (= 0 n)
        0 
        (if (= 1 n)
        1
        (+ (fibo (- n 1))(fibo (- n 2)))
        )
    )
)
(fibo 10)) 55 #'=)
)