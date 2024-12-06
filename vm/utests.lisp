(require "vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()
  ;; Appeler toutes les fonctions de test
  (test-exec-move);2 Tests
  (test-exec-store);4 Test
  (test-exec-load);2 Test
  (test-exec-incr);1 Test
  (test-exec-decr);1 Test
  (test-exec-add);2 Tests
  (test-exec-sub);2 Tests
  (test-exec-mul);2 Tests
  (test-exec-div);2 Tests
  (test-exec-jmp);1 Test
  (test-exec-jgt);2 Tests
  (test-exec-jge);3 Tests
  (test-exec-jlt);2 Tests
  (test-exec-jle);3 Tests
  (test-exec-jeq);2 Tests
  (test-exec-jsr);2 Tests
  (test-exec-rtn);2 Test
  (test-exec-push);2 Tests
  (test-exec-pop);1 Test
  (test-exec-cmp);12 Tests
  (test-exec-funcall);2 Tests
  ;; Afficher les résultats
  (format t "~%Nombre total de tests réussis : ~D/52~%" test-count)
)
(defun vmTest (code expected-value comparator &optional (todoBefore (lambda () ())) (result-getter (lambda () (get 'vm :R0))) (check-cmp-flags nil))
  "Fonction générique pour exécuter un cas de test unitaire."
  (format t "~%Test d'exécution de '~A'~%" code)
  (make-vm NIL) ; VM sans affichage mémoire initial
  (vm-load code NIL)
  (setf (get 'vm :R0) 1 (get 'vm :R1) 2) ; Initialisation de R0 à 1 et R2 à 2
  (funcall todobefore)
  (vm-exec)
  (cond
    ((and (listp expected-value) (= (length expected-value) 3));;CMP
      (if (and (= (get 'vm :EQ) (first expected-value))
                   (= (get 'vm :LT) (second expected-value))
                   (= (get 'vm :GT) (third expected-value)))
          (progn
            (setq test-count (+ test-count 1))
            (format t "~%Test d'exécution de '~A' réussi, valeur attendue (EQ: ~A, LT: ~A, GT: ~A), obtenue : (EQ: ~A, LT: ~A, GT: ~A)~%" code (first expected-value) (second expected-value) (third expected-value) (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
          (format t "~%Test d'exécution de '~A' échoué, valeur attendue (EQ: ~A, LT: ~A, GT: ~A), obtenue : (EQ: ~A, LT: ~A, GT: ~A)~%" code (first expected-value) (second expected-value) (third expected-value) (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT))))
    
    (t (let ((actual-value (funcall result-getter))) ; Actual value = soit R0 soit une valeur passée en paramètre de VMTest
      (if (funcall comparator actual-value expected-value)
      (progn
        (setq test-count (+ test-count 1))
        (format t "~%Test d'exécution de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" code expected-value actual-value))
      (format t "~%Test d'exécution de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" code expected-value actual-value))))
  )
)
(defun test-exec-move ()
  (vmTest '((MOVE 5 :R0)) 5 #'=)
  (vmTest '((MOVE :R1 :R0)) 2 #'=)
)
(defun test-exec-store ()
  (vmTest '((STORE :R1 1)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vmTest '((STORE 2 1)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vmTest '((STORE 2 :R0)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vmTest '((STORE :R1 :R0)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
)
(defun test-exec-load ()
  (vmTest '((LOAD 1 :R0)) 2 #'= (lambda () (set-to-vm-mem 'vm 1 2)))
  (vmTest '((LOAD :R0 :R0)) 2 #'= (lambda () (set-to-vm-mem 'vm 1 2)))
)
(defun test-exec-incr ()
  (vmTest '((INCR :R0)) 2 #'=)
)
(defun test-exec-decr ()
  (vmTest '((DECR :R0)) 0 #'=)
)
(defun test-exec-add ()
  (vmTest '((ADD 5 :R0)) 6 #'=)
  (vmTest '((ADD :R1 :R0)) 3 #'=)
)
(defun test-exec-sub ()
  (vmTest '((SUB 1 :R0)) 0 #'=)
  (vmTest '((SUB :R1 :R0)) -1 #'=)
)
(defun test-exec-mul ()
  (vmTest '((MUL 5 :R0)) 5 #'=)
  (vmTest '((MUL :R1 :R0)) 2 #'=)
)
(defun test-exec-div ()
  (vmTest '((DIV 5 :R0)) 1/5 #'=)
  (vmTest '((DIV :R1 :R0)) 1/2 #'=)
)
(defun test-exec-jmp ()
  (vmTest '((JMP label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
)

(defun test-exec-jgt ()
  (vmTest '((CMP 2 1)(JGT label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 1 2)(JGT label1)(MOVE 2 :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jge ()
  (vmTest '((CMP 2 1)(JGE label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 1 1)(JGE label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 1 2)(JGE label1)(MOVE 2 :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jlt ()
  (vmTest '((CMP 1 2)(JLT label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 2 1)(JLT label1)(MOVE 2 :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jle ()
  (vmTest '((CMP 1 2)(JLE label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 1 1)(JLE label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 2 1)(JLE label1)(MOVE 2 :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jeq ()
  (vmTest '((CMP 1 1)(JEQ label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)
  (vmTest '((CMP 2 1)(JEQ label1)(MOVE 2 :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jsr ()
  ;Test que la valeur de retour est bien mise sur la pile
  (vmTest '((JSR label1)(MOVE 2 :R0)(LABEL label1)) 501 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
  ;Test que le saut s'effectue bien
  (vmTest '((JSR label1)(MOVE 2 :R0)(LABEL label1)) 1 #'=)

)
(defun test-exec-rtn ()
    ;;Test que rtn retourne au bon endroit
    (vmtest '((JMP finfonction1)(LABEL fonction1)(MOVE 2 :R0)(RTN)(LABEL inateignable)(MOVE 3 :R0)(LABEL finfonction1)(JSR fonction1)(MOVE 3 :R0)) 3 #'=)
    ;Test que la fonction appellée avec JSR s'execute bien et que le rtn saute par dessus le label inateignable
    (vmtest '((JMP finfonction1)(LABEL fonction1)(MOVE 2 :R0)(RTN)(LABEL inateignable)(MOVE 3 :R0)(LABEL finfonction1)(JSR fonction1)) 2 #'=)
)
(defun test-exec-push ()
  (vmtest '((PUSH 3)) 3 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
  (vmtest '((PUSH :R1)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
)
(defun test-exec-pop ()
  (vmtest '((PUSH 3)(POP :R0)) 3 #'=)
)
(defun test-exec-cmp ()
  ;Cas inférieurs
  (vmtest '((CMP 3 5)) '(0 1 0) #'=)
  (vmtest '((CMP :R0 :R1)) '(0 1 0) #'=)
  (vmtest '((CMP 1 :R1)) '(0 1 0) #'=)
  (vmtest '((CMP :R0 2)) '(0 1 0) #'=)
  ;Cas supérieurs
  (vmtest '((CMP 5 3)) '(0 0 1) #'=)
  (vmtest '((CMP :R1 :R0)) '(0 0 1) #'=)
  (vmtest '((CMP :R1 1)) '(0 0 1) #'=)
  (vmtest '((CMP 2 :R0)) '(0 0 1) #'=)
  ;Cas égalités
  (vmtest '((CMP 1 1)) '(1 0 0) #'=)
  (vmtest '((CMP :R3 :R0)) '(1 0 0) #'= (lambda () (setf (get 'vm :R3) 1)));;On set exceptionnellement R3 avant l'exécution
  (vmtest '((CMP :R0 1)) '(1 0 0) #'=)
  (vmtest '((CMP 1 :R0)) '(1 0 0) #'=)
)
(defun test-exec-funcall ()
  (vmtest '((PUSH 3) (PUSH (1 2 3 4 5 6)) (MOVE :SP :FP) (PUSH 2)  (FUNCALL member)) '(3 4 5 6) #'equal)
  (vmtest '((PUSH (6 2 3 4 5)) (MOVE :SP :FP) (PUSH 1)  (FUNCALL car)) 6 #'=)
)