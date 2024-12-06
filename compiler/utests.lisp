(require "compiler.lisp")
(require "compiler/../vm/vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()
    (test-if);2 Tests
    (test-comparators);13 Tests
    (test-arithmetic-operators);5 Tests
    (format t "~%Nombre total de tests réussis : ~D/20~%" test-count)
  
)

(defun compTest (code expected-value comparator)
  "Fonction générique d'un cas de test unitaire"
  (format t "~%Test de compilation de '~A'~%" code)
  (make-vm NIL);VM sans affichage mémoire initial
  (vm-load (compilation code) NIL)
  (vm-exec)
  (if (funcall comparator (get 'vm :R0) expected-value)
    (progn
      (setq test-count (+ test-count 1))
      (format t "~%Test de compilation de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
    (format t "~%Test de compilation de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
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
(defun test-comparators ()
    (compTest '(= 1 1) T #'eql)
    (compTest '(= 0 1) NIL #'eql)
    (compTest '(< 1 2) T #'eql)
    (compTest '(< 2 1) NIL #'eql)
    (compTest '(> 2 1) T #'eql)
    (compTest '(> 1 2) NIL #'eql)
    (compTest '(>= 2 2) T #'eql)
    (compTest '(>= 2 1) T #'eql)
    (compTest '(>= 1 2) NIL #'eql)
    (compTest '(<= 2 2) T #'eql)
    (compTest '(<= 1 2) T #'eql)
    (compTest '(<= 2 1) NIL #'eql)
    (compTest '(= (+ 1 1) (+ 1 1)) T #'eql)
)