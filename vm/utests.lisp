(require "vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()

  ;; Appeler toutes les fonctions de test
  (test-exec-move);2 Tests
  (test-exec-store);2 Test
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
  (test-exec-jsr);1 Test
  (test-exec-rtn);1 Test
  (test-exec-push);2 Tests
  (test-exec-pop);1 Test
  (test-exec-cmp);12 Tests
  (test-exec-funcall);2 Tests
  (test)
  ;; Afficher les résultats
  (format t "~%Nombre total de tests réussis : ~D/48~%" test-count)
)

(defun increment-test-count ()
  (setq test-count (+ test-count 1)))

(defun test-exec-move ()
    ;; Cas où src est un littéral
    (make-vm)
    (setf (get 'vm :R1) 10)
    (vm-load '((MOVE 5 :R1)))
    (vm-exec)
    (if (eq (get 'vm :R1) 5)
        (progn
        (increment-test-count)
        (format t "~%Test 'MOVE 5 :R1 (src littéral)' réussi.~%"))
        (format t "~%Test 'MOVE 5 :R1 (src littéral)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))

    ;; Cas où src est un registre
    (make-vm)
    (setf (get 'vm :R1) 10 (get 'vm :R2) 5)
    (vm-load '((MOVE :R2 :R1)))
    (vm-exec)
    (if (eq (get 'vm :R1) 5)
        (progn
        (increment-test-count)
        (format t "~%Test 'MOVE :R2 :R1 (src registre)' réussi.~%"))
        (format t "~%Test 'MOVE :R2 :R1 (src registre)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))
)
(defun test-exec-store ()
  ;; Cas où dest est un littéral
  (make-vm)
  (setf (get 'vm :R1) 42)
  (vm-load '((STORE :R1 10)))
  (vm-exec)
  (if (eq (get-from-vm-mem 'vm 10) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'STORE :R1 10 (dest littéral)' réussi.~%"))
      (format t "~%Test 'STORE :R1 10 (dest littéral)' échoué, valeur attendue : 42, obtenue : ~A.~%" (get-from-vm-mem 'vm 10)))
      ;; Cas où dest est un littéral
  (make-vm)
  (setf (get 'vm :R1) 42 (get 'vm :R2) 10)
  (vm-load '((STORE :R1 :R2)))
  (vm-exec)
  (if (eq (get-from-vm-mem 'vm 10) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'STORE :R1 :R2 (dest registre)' réussi.~%"))
      (format t "~%Test 'STORE :R1 :R2 (dest registre)' échoué, valeur attendue : 42, obtenue : ~A.~%" (get-from-vm-mem 'vm 10)))
)

(defun test-exec-load ()
  (make-vm)
  (set-to-vm-mem 'vm 10 42)
  (vm-load '((LOAD 10 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'LOAD 10 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'LOAD 10 :R1 (src littéral)' échoué, valeur attendue : 42, obtenue : ~A.~%" (get 'vm :R1)))
  (make-vm)
  (set-to-vm-mem 'vm 10 42)
  (setf (get 'vm :R2) 10)
  (vm-load '((LOAD :R2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'LOAD :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'LOAD :R2 :R1 (src registre)' échoué, valeur attendue : 42, obtenue : ~A.~%" (get 'vm :R1)))
)

(defun test-exec-incr ()
  (make-vm)
  (setf (get 'vm :R1) 0)
  (vm-load '((INCR :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 1)
      (progn
        (increment-test-count)
        (format t "~%Test 'INCR :R1' réussi.~%"))
      (format t "~%Test 'INCR :R1' échoué, valeur attendue : 1, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-decr ()
  (make-vm)
  (setf (get 'vm :R1) 1)
  (vm-load '((DECR :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 0)
      (progn
        (increment-test-count)
        (format t "~%Test 'DECR :R1' réussi.~%"))
      (format t "~%Test 'DECR :R1' échoué, valeur attendue : 0, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-add ()
  ;; Cas où src est un littéral
  (make-vm)
  (setf (get 'vm :R1) 10)
  (vm-load '((ADD 5 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 15)
      (progn
        (increment-test-count)
        (format t "~%Test 'ADD 5 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'ADD 5 :R1 (src littéral)' échoué, valeur attendue : 15, obtenue : ~A.~%" (get 'vm :R1)))
  
  ;; Cas où src est un registre
  (make-vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 5)
  (vm-load '((ADD :R2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 15)
      (progn
        (increment-test-count)
        (format t "~%Test 'ADD :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'ADD :R2 :R1 (src registre)' échoué, valeur attendue : 15, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-sub ()
  ;; Cas où src est un littéral
  (make-vm)
  (setf (get 'vm :R1) 10)
  (vm-load '((SUB 3 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 7)
      (progn
        (increment-test-count)
        (format t "~%Test 'SUB 3 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'SUB 3 :R1 (src littéral)' échoué, valeur attendue : 7, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 3)
  (vm-load '((SUB :R2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 7)
      (progn
        (increment-test-count)
        (format t "~%Test 'SUB :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'SUB :R2 :R1 (src registre)' échoué, valeur attendue : 7, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-mul ()
  ;; Cas où src est un littéral
  (make-vm)
  (setf (get 'vm :R1) 4)
  (vm-load '((MUL 3 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 12)
      (progn
        (increment-test-count)
        (format t "~%Test 'MUL 3 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'MUL 3 :R1 (src littéral)' échoué, valeur attendue : 12, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm)
  (setf (get 'vm :R1) 4 (get 'vm :R2) 3)
  (vm-load '((MUL :R2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 12)
      (progn
        (increment-test-count)
        (format t "~%Test 'MUL :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'MUL :R2 :R1 (src registre)' échoué, valeur attendue : 12, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-div ()
  ;; Cas où src est un littéral
  (make-vm)
  (setf (get 'vm :R1) 10)
  (vm-load '((DIV 2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 5)
      (progn
        (increment-test-count)
        (format t "~%Test 'DIV 2 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'DIV 2 :R1 (src littéral)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 2)
  (vm-load '((DIV :R2 :R1)))
  (vm-exec)
  (if (eq (get 'vm :R1) 5)
      (progn
        (increment-test-count)
        (format t "~%Test 'DIV :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'DIV :R2 :R1 (src registre)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-jmp ()
  (make-vm)
  (setf (get 'vm :R1) 0)
  (vm-load '((JMP label1)(MOVE 1 :R1)(LABEL label1)))
  (vm-exec)
  (if (= (get 'vm :R1) 0)
      (progn
        (increment-test-count)
        (format t "~%Test JMP passé"))
      (format t "~%Test JMP échoué")))

(defun test-exec-jgt ()
    ;;Test que le saut s'effectue bien si le flag GT est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 1)

    (vm-load '((JGT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGT passé"))
        (format t "~%Test JGT échoué"))
    ;;Test que le saut ne s'effectue pas si le flag GT est à 0
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 0)

    (vm-load '((JGT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JGT passé"))
        (format t "~%Test JGT échoué"))
)


(defun test-exec-jge ()
    ;;Test que le saut s'effectue bien si le flag GT est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 1)

    (vm-load '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)

    (vm-load '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
    ;;Test que le saut ne s'effectue pas si les flags GT et EQ sont à 0
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 0)
    (setf (get 'vm :EQ) 0)

    (vm-load '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
)

(defun test-exec-jlt ()
    ;;Test que le saut s'effectue bien si le flag LT est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 1)

    (vm-load '((JLT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLT passé"))
        (format t "~%Test JLT échoué"))
    ;;Test que le saut ne s'effectue pas si le flag LT est à 0
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 0)

    (vm-load '((JLT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JLT passé"))
        (format t "~%Test JLT échoué"))
)
(defun test-exec-jle ()
    ;;Test que le saut s'effectue bien si le flag LT est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 1)

    (vm-load '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)

    (vm-load '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
    ;;Test que le saut ne s'effectue pas si les flags LT et EQ sont à 0
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 0)
    (setf (get 'vm :EQ) 0)

    (vm-load '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
)


(defun test-exec-jeq ()
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)
    (vm-load '((JEQ label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JEQ passé"))
        (format t "~%Test JEQ échoué"))
    ;;Test que le saut ne s'effectue pas si le flag EQ est à 0
    (make-vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 0)
    (vm-load '((JEQ label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JEQ passé"))
        (format t "~%Test JEQ échoué"))
)

(defun test-exec-jsr ()
    ;;Test que le saut s'effectue bien et que l'adresse de retour est sur la pile
    (make-vm)
    (setf (get 'vm :R1) 0)
    (vm-load '((JSR label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec)
    (if (and (= (get 'vm :R1) 0) (= (get-from-vm-mem 'vm (- (get 'vm :SP) 1 )) (get 'vm :startCode)))
        (progn
            (increment-test-count)
            (format t "~%Test JSR passé"))
        (format t "~%Test JSR échoué")
     )
)

(defun test-exec-rtn ()
    ;;Test d'un cas typique d'appel de fonction avec RTN
    (make-vm)
    (vm-load '((JMP finfonction1)(LABEL fonction1)(MOVE 1 :R1)(RTN)(LABEL inateignable)(MOVE 1 :R2)(LABEL finfonction1)(JSR fonction1)(MOVE 1 :R3)))
    (vm-exec)
    (if (and (and (= (get 'vm :R1) 1) (= (get 'vm :R2) 0)) (= (get 'vm :R3) 1) )
            (progn
                (increment-test-count)
                (format t "~%Test RTN passé"))
            (format t "~%Test RTN échoué")
    )
)

(defun test-exec-push ()
  ;; Cas où src est un littéral
  (make-vm)
  (vm-load '((PUSH 3)))
  (vm-exec)
  (if (eq (get-from-vm-mem 'vm (- (get 'vm :SP) 1)) 3)
      (progn
        (increment-test-count)
        (format t "~%Test 'PUSH 3 (src littéral)' réussi.~%"))
      (format t "~%Test 'PUSH 3 (src littéral)' échoué, valeur attendue : 3, obtenue : ~A.~%" (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
  ;; Cas où src est un registre
  (make-vm)
  (setf (get 'vm :R1) 3)
  (vm-load '((PUSH :R1)))
  (vm-exec)
  (if (eq (get-from-vm-mem 'vm (- (get 'vm :SP) 1)) 3)
      (progn
        (increment-test-count)
        (format t "~%Test 'PUSH :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'PUSH :R1 (src registre)' échoué, valeur attendue : 3, obtenue : ~A.~%" (get-from-vm-mem 'vm (- (get 'vm :SP) 1))))
)

(defun test-exec-pop ()
  (make-vm)
  (vm-load '((PUSH 3)(POP :R1)))
  (vm-exec)
  (if (= (get 'vm :R1) 3) 
      (progn
        (increment-test-count)
        (format t "~%Test 'POP :R1' réussi.~%"))
      (format t "~%Test 'POP :R1' échoué, valeur attendue : 3, obtenue : ~A.~%" (get 'vm :R1)))
)
(defun test-exec-cmp ()
  ;; Cas où le premier argument est inférieur au second (littéraux)
  (make-vm)
  (vm-load '((CMP 3 5)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 1)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 3 5 (inférieur, littéraux)' réussi.~%"))
      (format t "~%Test 'CMP 3 5 (inférieur, littéraux)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument est inférieur au second (registres)
  (make-vm)
  (setf (get 'vm :R1) 5 (get 'vm :R2) 10)
  (vm-load '((CMP :R1 :R2)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 1)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R1 :R2 (inférieur, registres)' réussi.~%"))
      (format t "~%Test 'CMP :R1 :R2 (inférieur, registres)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument est un littéral inférieur au second un registre
  (make-vm)
  (setf (get 'vm :R2) 10)
  (vm-load '((CMP 5 :R2)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 1)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 5 :R2 (inférieur, littéral et registre)' réussi.~%"))
      (format t "~%Test 'CMP 5 :R2 (inférieur, littéral et registre)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument est un registre inférieur au second un littéral
  (make-vm)
  (setf (get 'vm :R2) 5)
  (vm-load '((CMP :R2 15)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 1)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R2 15 (inférieur, registre et littéral)' réussi.~%"))
      (format t "~%Test 'CMP :R2 15 (inférieur, registre et littéral)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument est supérieur au second (littéraux)
  (make-vm)
  (vm-load '((CMP 5 3)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 1))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 5 3 (supérieur, littéraux)' réussi.~%"))
      (format t "~%Test 'CMP 5 3 (supérieur, littéraux)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument est supérieur au second (registres)
  (make-vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 5)
  (vm-load '((CMP :R1 :R2)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 1))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R1 :R2 (supérieur, registres)' réussi.~%"))
      (format t "~%Test 'CMP :R1 :R2 (supérieur, registres)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
  ;; Cas où le premier argument registre est supérieur au second littéral
  (make-vm)
  (setf (get 'vm :R1) 7)
  (vm-load '((CMP :R1 5)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 1))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R1 5 (supérieur, registre et littéral)' réussi.~%"))
      (format t "~%Test 'CMP :R1 5 (supérieur, registre et littéral)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))
;; Cas où le premier argument littéral est supérieur au second registre
  (make-vm)
  (setf (get 'vm :R1) 5)
  (vm-load '((CMP 10 :R1)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 0)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 1))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 10 :R1 (supérieur, littéral et registre)' réussi.~%"))
      (format t "~%Test 'CMP 10 :R1 (supérieur, littéral et registre)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))

  ;; Cas où les deux arguments sont égaux (littéraux)
  (make-vm)
  (vm-load '((CMP 5 5)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 1)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 5 5 (égalité, littéraux)' réussi.~%"))
      (format t "~%Test 'CMP 5 5 (égalité, littéraux)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))

  ;; Cas où les deux arguments sont égaux (registres)
  (make-vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 10)
  (vm-load '((CMP :R1 :R2)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 1)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R1 :R2 (égalité, registre et registre)' réussi.~%"))
      (format t "~%Test 'CMP :R1 :R2 (égalité, registre et registre)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))

  ;; Cas où le premier argument est un littéral et le second un registre (égalité)
  (make-vm)
  (setf (get 'vm :R1) 5)
  (vm-load '((CMP 5 :R1)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 1)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP 5 :R1 (égalité, littéral et registre)' réussi.~%"))
      (format t "~%Test 'CMP 5 :R1 (égalité, littéral et registre)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))

  ;; Cas où le premier argument est un registre et le second un littéral (égalité)
  (make-vm)
  (setf (get 'vm :R1) 7)
  (vm-load '((CMP :R1 7)))
  (vm-exec)
  (if (and (= (get 'vm :EQ) 1)
          (= (get 'vm :LT) 0)
          (= (get 'vm :GT) 0))
      (progn
        (increment-test-count)
        (format t "~%Test 'CMP :R1 7 (égalité, registre et littéral)' réussi.~%"))
      (format t "~%Test 'CMP :R1 7 (égalité, registre et littéral)' échoué, EQ: ~A, LT: ~A, GT: ~A.~%" 
              (get 'vm :EQ) (get 'vm :LT) (get 'vm :GT)))

)

(defun test-exec-funcall ()
  (make-vm)
  (vm-load '((PUSH 3) (PUSH (1 2 3 4 5)) (MOVE :SP :FP) (PUSH 2)  (FUNCALL member)))
  (vm-exec)
  (vm-print)
  (if (equal (get 'vm :R0) '(3 4 5))
    (progn
      (increment-test-count)
      (format t "~%Test 'FUNCALL member avec 3 (1 2 3 4 5) sur la pile' réussi.~%"))
    (format t "~%Test 'FUNCALL member avec 3 (1 2 3 4 5) sur la pile' échoué, valeur attendue : (3 4 5), obtenue : ~A.~%" (get 'vm :R1)))
  (make-vm)
  (vm-load '((PUSH (1 2 3 4 5)) (MOVE :SP :FP) (PUSH 1)  (FUNCALL car)))
  (vm-exec)
  (vm-print)
  (if (eql (get 'vm :R0) 1)
    (progn
      (increment-test-count)
      (format t "~%Test 'FUNCALL car avec (1 2 3 4 5) sur la pile' réussi.~%"))
    (format t "~%Test 'FUNCALL car avec (1 2 3 4 5) sur la pile' échoué, valeur attendue : (3 4 5), obtenue : ~A.~%" (get 'vm :R1)))
)
(defun test ()
  (make-vm)
  (vm-load '((MOVE T :R0)))
  (vm-exec)
  (vm-print)
)
