(require "./../vm/vm.lisp")
(require "./../compiler/compiler.lisp")

(defun run-all-tests ()
  (setq test-count 0)
  (run-vm-tests T);54 Tests
  (run-comp-tests T);52 Tests
  (format t "~%Nombre total de tests réussis : ~D/106~%" test-count)
)
(defun test-comp-comp ()
  (make-vm NIL 'vm 100000);VM sans affichage mémoire initial
  (compile-fichier "./../compiler/compiler.lisp" "compiler.asm")
  (vm-load-file "compiler.asm")
  (vm-exec)

)
(defun run-comp-tests (&optional (run-all NIL))
  ;52 Tests
  (setq test-comp-count 0)
  (test-if);2 Tests
  (test-comparators);13 Tests
  (test-arithmetic-operators);5 Tests
  (test-progn);1 Test
  (test-cond);2 Tests
  (test-and-or);12 Tests
  (test-fonctions);5 Tests
  (test-let);2 Tests
  (test-setq);1 Test
  (test-while);4 Tests
  (test-file-compilation);2 Tests
  (test-letstar);1 Test
  (test-lisp-fonctions);2 Tests
  (if run-all
    (setq test-count (+ test-count test-comp-count))
  )
  (format t "~%Nombre total de tests réussis pour la compilation: ~D/52~%" test-comp-count)
)
(defun comp-file-test (file-name-in file-name-out expected-value comparator)
  "Fonction générique d'un cas de test unitaire de compilation de fichier lisp vers assembleur"
  (make-vm NIL 'vm 500);VM sans affichage mémoire initial
  (compile-fichier file-name-in file-name-out)
  (vm-load-file file-name-out)
  (vm-exec)
  (if (funcall comparator (get 'vm :R0) expected-value)
    (progn
      (setq test-comp-count (+ test-comp-count 1))
      (format t "~%Test de compilation de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" file-name-in expected-value (get 'vm :R0)))
    (format t "~%Test de compilation de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" file-name-in expected-value (get 'vm :R0)))
)
(defun test-file-compilation ()
  (comp-file-test "factorial.lisp" "factorial.asm" 720 #'=)
  (comp-file-test "fibonacci.lisp" "fibonacci.asm" 55 #'=)

)
(defun comp-test (code expected-value comparator)
  "Fonction générique d'un cas de test unitaire de compilation d'expression lisp"
  (format t "~%Test de compilation de '~A'~%" code)
  (make-vm NIL 'vm 500);VM sans affichage mémoire initial
  (vm-load (compilation code '() '()))
  (vm-exec)
  (if (funcall comparator (get 'vm :R0) expected-value)
    (progn
      (setq test-comp-count (+ test-comp-count 1))
      (format t "~%Test de compilation de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
    (format t "~%Test de compilation de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" code expected-value (get 'vm :R0)))
)
(defun test-backquote ()
  (comp-test '(progn (defun add (x) (let ((labelname "BLABLA")) (append `((LABEL ,labelname))))) (add 90) ) '((LABEL "BLABLA")) #'equal)     

)
(defun test-letstar ()
  (comp-test '(progn (defun add (x) (let* ((a 5) (b (+ a 6))) (+ a b))) (add 10)) 16 #'=)
)
(defun test-and-or ()
  (comp-test '(and (= 1 1) (= 2 2)) 1 #'=)
  (comp-test '(and (= 9 1) (= 2 2)) 0 #'=)
  (comp-test '(and (= 1 1) (= 3 2)) 0 #'=)
  (comp-test '(and (= 0 1) (= 3 2)) 0 #'=)
  (comp-test '(and (and (= 1 1) (= 1 1)) (= 2 2)) 1 #'=)

  (comp-test '(or (= 9 1) (= 2 2)) 1 #'=)
  (comp-test '(or (= 1 1) (= 4 2)) 1 #'=)
  (comp-test '(or (= 1 1) (= 2 2)) 1 #'=)
  (comp-test '(or (= 0 1) (= 1 2)) 0 #'=)
  (comp-test '(or (or (= 2 1) (= 1 1)) (= 1 2)) 1 #'=)

  (comp-test '(and (or (= 2 1) (= 1 1)) (= 1 1)) 1 #'=)
  (comp-test '(and (or (= 2 1) (= 1 1)) (= 2 1)) 0 #'=)
)

(defun test-arithmetic-operators ()
  (comp-test '(+ 1 1) 2 #'=)
  (comp-test '(- 3 1) 2 #'=)
  (comp-test '(* 1 2) 2 #'=)
  (comp-test '(/ 4 2) 2 #'=)
  (comp-test '(/ (+ (+ 5 5) (/ 4 2)) 6) 2 #'=)    
)
(defun test-if ()
  (comp-test '(if 1 100 2) 100 #'=)
  (comp-test '(if nil 100 2) 2 #'=)
)
(defun test-cond ()
  (comp-test '(cond ((< 10 0) (+ 1 1)) ((> 10 0) (+ 0 0))) 0 #'=)
  (comp-test '(cond ((< -10 0) (+ 1 1)) ((> 10 0) (+ 0 0))) 2 #'=)
)
(defun test-comparators ()
  (comp-test '(= 1 1) 1 #'=)
  (comp-test '(= 0 1) 0 #'=)
  (comp-test '(< 1 2) 1 #'=)
  (comp-test '(< 2 1) 0 #'=)
  (comp-test '(> 2 1) 1 #'=)
  (comp-test '(> 1 2) 0 #'=)
  (comp-test '(>= 2 2) 1 #'=)
  (comp-test '(>= 2 1) 1 #'=)
  (comp-test '(>= 1 2) 0 #'=)
  (comp-test '(<= 2 2) 1 #'=)
  (comp-test '(<= 1 2) 1 #'=)
  (comp-test '(<= 2 1) 0 #'=)
  (comp-test '(= (+ 1 1) (+ 1 1)) 1 #'=)
)
(defun test-while ()
  ;Boucle while incrémentation d'un counter jusqua 10
  (comp-test '(progn (defun add (x) (let ((counter 0)) (progn (loop while (< counter 10) do  (setq counter (+ counter 1))  ) (= counter 10)))) (add 90) ) 1 #'=)
  (comp-test '(progn (defun add (x) (let ((counter 0)) (progn (loop while (> counter 10) do  (setq counter (+ counter 1))) (= counter 0)))) (add 90) ) 1 #'=)
  (comp-test '(progn (defun add (x) (let ((counter 0)) (progn (loop while (< counter 10) do  (setq counter (+ counter 1)) (setq counter (+ counter 3)) ) (= counter 12)))) (add 90) ) 1 #'=)
  (comp-test '(progn (defun add (x) (let ((counter 0)) (progn (loop while (< counter 10) do  (setq counter (+ counter 1)) (setq counter (+ counter 3)) ) (= counter 11)))) (add 90) ) 0 #'=)

)
(defun test-progn ()
  (comp-test '(progn (+ 1 1) (+ 2 2)) 4 #'=)
)
(defun test-let ()
  ;Simple
  (comp-test '(progn (defun add (x) (let ((a 10)) (+ a a)(+ a x))) (add 90)) 100 #'=)
  ;Nested, équivalent à un let*
  (comp-test '(progn (defun add (x) (let ((a 5)) (let ((b 3)) (+ (+ a b) x))))(add 10)) 18 #'=)
)
(defun test-setq ()
  ;Test de modification d'une variable locale UNIQUEMENT AU SEIN D'UN LET D'UNE FONCTION
  (comp-test '(progn (defun add (x) (let ((a 1)) (progn (setq a 55) (+ a x)))) (add 45)) 100 #'=)
)
(defun test-lisp-fonctions ()
  (comp-test '(car '(6 2 3 4 5)) 6 #'=)
  (comp-test '(member 3 '(6 2 3 4 5)) '(3 4 5) #'equal)
)
(defun test-fonctions ()
  (comp-test '(progn (defun add (x y) (+ x y))(add 1 2)) 3 #'=)
  (comp-test '(progn (defun add (x y) (+ x y))(add 8 2)) 10 #'=)
  (comp-test '(progn (defun fact (n) (if (= n 1) 1 (* n (fact (- n 1))))) (fact 6)) 720 #'=)
  (comp-test '(progn (defun add (x y) (+ x y))(add (add 6 4) 3)) 13 #'=)
  (comp-test '(progn (defun fibo (n)(if (= 0 n) 0 (if (= 1 n) 1 (+ (fibo (- n 1))(fibo (- n 2))))))(fibo 10)) 55 #'=)
)

(defun run-vm-tests (&optional (run-all NIL))
  ;54 tests
  (setq test-vm-count 0)
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
  (test-exec-jne);2 Tests
  (test-exec-jsr);2 Tests
  (test-exec-rtn);2 Test
  (test-exec-push);2 Tests
  (test-exec-pop);1 Test
  (test-exec-cmp);12 Tests
  (test-exec-funcall);2 Tests
  (if run-all
    (setq test-count (+ test-count test-vm-count))
  )
  (format t "~%Nombre total de tests réussis pour la VM: ~D/54~%" test-vm-count)
)
(defun vm-test (code expected-value comparator &optional (todoBefore (lambda () ())) (result-getter (lambda () (get 'vm :R0))) (check-cmp-flags nil))
  "Fonction générique pour exécuter un cas de test unitaire de code assembleur avec une VM"
  (format t "~%Test d'exécution de '~A'~%" code)
  (make-vm NIL 'vm 500) ; VM sans affichage mémoire initial
  (vm-load code)
  (setf (get 'vm :R0) 1 (get 'vm :R1) 2) ; Initialisation de R0 à 1 et R2 à 2
  (funcall todobefore)
  (vm-exec)
  (cond
    ((and (listp expected-value) (= (length expected-value) 3));;CMP
      (if (and (= (get 'vm :EQ) (first expected-value))
                   (= (get 'vm :LT) (second expected-value))
                   (= (get 'vm :GT) (third expected-value)))
          (progn
            (setq test-vm-count (+ test-vm-count 1))
            (format t "~%Test d'exécution de '~A' réussi, valeur attendue (EQ: ~A, LT: ~A, GT: ~A), obtenue : (EQ: ~A, LT: ~A, GT: ~A)~%" code (first expected-value) (second expected-value) (third expected-value) (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
          (format t "~%Test d'exécution de '~A' échoué, valeur attendue (EQ: ~A, LT: ~A, GT: ~A), obtenue : (EQ: ~A, LT: ~A, GT: ~A)~%" code (first expected-value) (second expected-value) (third expected-value) (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT))))
    
    (t (let ((actual-value (funcall result-getter))) ; Actual value = soit R0 soit une valeur passée en paramètre de vm-test
      (if (funcall comparator actual-value expected-value)
      (progn
        (setq test-vm-count (+ test-vm-count 1))
        (format t "~%Test d'exécution de '~A' réussi, valeur attendue : ~A, obtenue : ~A.~%" code expected-value actual-value))
      (format t "~%Test d'exécution de '~A' échoué, valeur attendue : ~A, obtenue : ~A.~%" code expected-value actual-value))))
  )
)
(defun test-exec-move ()
  (vm-test '((MOVE (LIT 5) :R0)) 5 #'=)
  (vm-test '((MOVE :R1 :R0)) 2 #'=)
)
(defun test-exec-store ()
  (vm-test '((STORE :R1 (LIT 1))) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vm-test '((STORE (LIT 2) (LIT 1))) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vm-test '((STORE (LIT 2) :R0)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
  (vm-test '((STORE :R1 :R0)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm 1)))
)
(defun test-exec-load ()
  (vm-test '((LOAD (LIT 1) :R0)) 2 #'= (lambda () (set-to-vm-mem 'vm 1 2)))
  (vm-test '((LOAD :R0 :R0)) 2 #'= (lambda () (set-to-vm-mem 'vm 1 2)))
)
(defun test-exec-incr ()
  (vm-test '((INCR :R0)) 2 #'=)
)
(defun test-exec-decr ()
  (vm-test '((DECR :R0)) 0 #'=)
)
(defun test-exec-add ()
  (vm-test '((ADD (LIT 5) :R0)) 6 #'=)
  (vm-test '((ADD :R1 :R0)) 3 #'=)
)
(defun test-exec-sub ()
  (vm-test '((SUB (LIT 1) :R0)) 0 #'=)
  (vm-test '((SUB :R1 :R0)) -1 #'=)
)
(defun test-exec-mul ()
  (vm-test '((MUL (LIT 5) :R0)) 5 #'=)
  (vm-test '((MUL :R1 :R0)) 2 #'=)
)
(defun test-exec-div ()
  (vm-test '((DIV (LIT 5) :R0)) 1/5 #'=)
  (vm-test '((DIV :R1 :R0)) 1/2 #'=)
)
(defun test-exec-jmp ()
  (vm-test '((JMP label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
)

(defun test-exec-jgt ()
  (vm-test '((CMP (LIT 2) (LIT 1))(JGT label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 1) (LIT 2))(JGT label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jge ()
  (vm-test '((CMP (LIT 2) (LIT 1))(JGE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 1) (LIT 1))(JGE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 1) (LIT 2))(JGE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jlt ()
  (vm-test '((CMP (LIT 1) (LIT 2))(JLT label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 2) (LIT 1))(JLT label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jle ()
  (vm-test '((CMP (LIT 1) (LIT 2))(JLE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 1) (LIT 1))(JLE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 2) (LIT 1))(JLE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jeq ()
  (vm-test '((CMP (LIT 1) (LIT 1))(JEQ label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
  (vm-test '((CMP (LIT 2) (LIT 1))(JEQ label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
)
(defun test-exec-jne ()
  (vm-test '((CMP (LIT 1) (LIT 1))(JNE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 2 #'=)
  (vm-test '((CMP (LIT 2) (LIT 1))(JNE label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)
)
(defun test-exec-jsr ()
  ;Test que la valeur de retour est bien mise sur la pile
  (vm-test '((JSR label1)(MOVE (LIT 2) :R0)(LABEL label1)) 251 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
  ;Test que le saut s'effectue bien
  (vm-test '((JSR label1)(MOVE (LIT 2) :R0)(LABEL label1)) 1 #'=)

)
(defun test-exec-rtn ()
    ;;Test que rtn retourne au bon endroit
    (vm-test '((JMP finfonction1)(LABEL fonction1)(MOVE (LIT 2) :R0)(RTN)(LABEL inateignable)(MOVE (LIT 3) :R0)(LABEL finfonction1)(JSR fonction1)(MOVE (LIT 3) :R0)) 3 #'=)
    ;Test que la fonction appellée avec JSR s'execute bien et que le rtn saute par dessus le label inateignable
    (vm-test '((JMP finfonction1)(LABEL fonction1)(MOVE (LIT 2) :R0)(RTN)(LABEL inateignable)(MOVE (LIT 3) :R0)(LABEL finfonction1)(JSR fonction1)) 2 #'=)
)
(defun test-exec-push ()
  (vm-test '((PUSH (LIT 3))) 3 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
  (vm-test '((PUSH :R1)) 2 #'= (lambda () ()) (lambda () (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
)
(defun test-exec-pop ()
  (vm-test '((PUSH (LIT 3))(POP :R0)) 3 #'=)
)
(defun test-exec-cmp ()
  ;Cas inférieurs
  (vm-test '((CMP (LIT 3) (LIT 5))) '(0 1 0) #'=)
  (vm-test '((CMP :R0 :R1)) '(0 1 0) #'=)
  (vm-test '((CMP (LIT 1) :R1)) '(0 1 0) #'=)
  (vm-test '((CMP :R0 (LIT 2))) '(0 1 0) #'=)
  ;Cas supérieurs
  (vm-test '((CMP (LIT 5) (LIT 3))) '(0 0 1) #'=)
  (vm-test '((CMP :R1 :R0)) '(0 0 1) #'=)
  (vm-test '((CMP :R1 (LIT 1))) '(0 0 1) #'=)
  (vm-test '((CMP (LIT 2) :R0)) '(0 0 1) #'=)
  ;Cas égalités
  (vm-test '((CMP (LIT 1) (LIT 1))) '(1 0 0) #'=)
  (vm-test '((CMP :R3 :R0)) '(1 0 0) #'= (lambda () (setf (get 'vm :R3) 1)));;On set exceptionnellement R3 avant l'exécution
  (vm-test '((CMP :R0 (LIT 1))) '(1 0 0) #'=)
  (vm-test '((CMP (LIT 1) :R0)) '(1 0 0) #'=)
)
(defun test-exec-funcall ()
  (vm-test '( (PUSH  (LIT (1 2 3 4 5 6))) (PUSH (LIT 3)) (MOVE :SP :FP) (PUSH (LIT 2))  (FUNCALL member)) '(3 4 5 6) #'equal)
  (vm-test '((PUSH (LIT (6 2 3 4 5))) (MOVE :SP :FP) (PUSH (LIT 1))  (FUNCALL car)) 6 #'=)
)