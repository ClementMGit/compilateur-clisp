(require "compiler.lisp")
(require "compiler/../vm/vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()
    (test-if);2 Tests
    (test-comparators);13 Tests
    (test-arithmetic-operators);5 Tests
    (format t "~%Nombre total de tests réussis : ~D/20~%" test-count)
  
)

(defun increment-test-count ()
  (setq test-count (+ test-count 1))
)
(defun test-arithmetic-operators ()
    (make-vm)
    (vm-load (compilation '(+ 1 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) 2)
      (progn
        (increment-test-count)
        (format t "~%Test '(+ 1 1)' réussi.~%"))
      (format t "~%Test '(+ 1 1)' échoué, valeur attendue : 2, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(- 3 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) 2)
      (progn
        (increment-test-count)
        (format t "~%Test '(- 3 1)' réussi.~%"))
      (format t "~%Test '(- 3 1)' échoué, valeur attendue : 2, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(* 1 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) 2)
      (progn
        (increment-test-count)
        (format t "~%Test '(* 1 2)' réussi.~%"))
      (format t "~%Test '(* 1 2)' échoué, valeur attendue : 2, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(/ 4 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) 2)
      (progn
        (increment-test-count)
        (format t "~%Test '(/ 4 2)' réussi.~%"))
      (format t "~%Test '(/ 4 2)' échoué, valeur attendue : 2, obtenue : ~A.~%" (get 'vm :R0)))
    ;Test nested op
    (make-vm)
    (vm-load (compilation '(/ (+ (+ 5 5) (/ 4 2) ) 6)))
    (vm-exec)
    (if (eql (get 'vm :R0) 2)
      (progn
        (increment-test-count)
        (format t "~%Test '(/ (+ (+ 5 5) (/ 4 2) ) 6)' réussi.~%"))
      (format t "~%Test '(/ (+ (+ 5 5) (/ 4 2) ) 6)' échoué, valeur attendue : 2, obtenue : ~A.~%" (get 'vm :R0)))
    
)
(defun test-if ()
    (make-vm)
    (vm-load (compilation '(if 10 100 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) 100)
      (progn
        (increment-test-count)
        (format t "~%Test 'if statement(condition vraie)' réussi.~%"))
      (format t "~%Test 'if statement(condition vraie)' échoué, valeur attendue : 100, obtenue : ~A.~%" (get 'vm :R0)))
    
    (make-vm)
    (vm-load (compilation '(if nil 100 50)))
    (vm-exec)
    (if (eql (get 'vm :R0) 50)
      (progn
        (increment-test-count)
        (format t "~%Test 'if statement(condition fausse)' réussi.~%"))
      (format t "~%Test 'if statement(condition fausse)' échoué, valeur attendue : 50, obtenue : ~A.~%" (get 'vm :R0)))
)
(defun test-comparators ()
    ;;Tests pour =
    (make-vm)
    (vm-load (compilation '(= 1 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
    (progn
    (increment-test-count)
    (format t "~%Test '(= 1 1)(condition vraie)' réussi.~%"))
    (format t "~%Test '(= 1 1)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))
    
    (make-vm)
    (vm-load (compilation '(= 0 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) NIL)
    (progn
    (increment-test-count)
    (format t "~%Test '(= 0 1)(condition fausse)' réussi.~%"))
    (format t "~%Test '(= 0 1)(condition fausse)' échoué, valeur attendue : NIL, obtenue : ~A.~%" (get 'vm :R0)))
        ;; Tests pour <
    (make-vm)
    (vm-load (compilation '(< 1 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(< 1 2)(condition vraie)' réussi.~%"))
        (format t "~%Test '(< 1 2)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))

    (make-vm)
    (vm-load (compilation '(< 3 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) NIL)
        (progn
        (increment-test-count)
        (format t "~%Test '(< 3 1)(condition fausse)' réussi.~%"))
        (format t "~%Test '(< 3 1)(condition fausse)' échoué, valeur attendue : NIL, obtenue : ~A.~%" (get 'vm :R0)))

    ;; Tests pour >
    (make-vm)
    (vm-load (compilation '(> 3 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(> 3 2)(condition vraie)' réussi.~%"))
        (format t "~%Test '(> 3 2)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))

    (make-vm)
    (vm-load (compilation '(> 1 3)))
    (vm-exec)
    (if (eql (get 'vm :R0) NIL)
        (progn
        (increment-test-count)
        (format t "~%Test '(> 1 3)(condition fausse)' réussi.~%"))
        (format t "~%Test '(> 1 3)(condition fausse)' échoué, valeur attendue : NIL, obtenue : ~A.~%" (get 'vm :R0)))

    ;; Tests pour <=
    (make-vm)
    (vm-load (compilation '(<= 2 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(<= 2 2)(condition vraie)' réussi.~%"))
        (format t "~%Test '(<= 2 2)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(<= 1 2)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(<= 1 2)(condition vraie)' réussi.~%"))
        (format t "~%Test '(<= 1 2)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(<= 3 1)))
    (vm-exec)
    (if (eql (get 'vm :R0) NIL)
        (progn
        (increment-test-count)
        (format t "~%Test '(<= 3 1)(condition fausse)' réussi.~%"))
        (format t "~%Test '(<= 3 1)(condition fausse)' échoué, valeur attendue : NIL, obtenue : ~A.~%" (get 'vm :R0)))

    ;; Tests pour >=
    (make-vm)
    (vm-load (compilation '(>= 3 3)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(>= 3 3)(condition vraie)' réussi.~%"))
        (format t "~%Test '(>= 3 3)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(>= 4 3)))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(>= 4 3)(condition vraie)' réussi.~%"))
        (format t "~%Test '(>= 4 3)(condition vraie)' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))
    (make-vm)
    (vm-load (compilation '(>= 1 3)))
    (vm-exec)
    (if (eql (get 'vm :R0) NIL)
        (progn
        (increment-test-count)
        (format t "~%Test '(>= 1 3)(condition fausse)' réussi.~%"))
        (format t "~%Test '(>= 1 3)(condition fausse)' échoué, valeur attendue : NIL, obtenue : ~A.~%" (get 'vm :R0)))
    ;Test nested expr
    (make-vm)
    (vm-load (compilation '(= (+ 1 1) (+ 1 1))))
    (vm-exec)
    (if (eql (get 'vm :R0) T)
        (progn
        (increment-test-count)
        (format t "~%Test '(= (+ 1 1) (+ 1 1))' réussi.~%"))
        (format t "~%Test '(= (+ 1 1) (+ 1 1))' échoué, valeur attendue : T, obtenue : ~A.~%" (get 'vm :R0)))

)