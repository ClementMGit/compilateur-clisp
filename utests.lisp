(require "vm.lisp")
(setq test-count 0)  ; Compteur global des tests réussis

(defun run-tests ()
  (let ((nbTests 29)) ;; Nombre total de tests
    ;; Appeler toutes les fonctions de test
    (test-exec-move);2 Tests
    (test-exec-store);1 Test
    (test-exec-load);1 Test
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
    ;;TODO CMP PUSH POP HALT
    ;; Afficher les résultats
    (format t "~%Nombre total de tests réussis : ~D/~D~%" test-count nbtests )
  )
)

(defun increment-test-count ()
  (setq test-count (+ test-count 1)))

(defun test-exec-move ()
    ;; Cas où src est un littéral
    (make-vm 'vm)
    (setf (get 'vm :R1) 10)
    (vm-load 'vm '((MOVE 5 :R1)))
    (vm-exec 'vm)
    (if (eq (get 'vm :R1) 5)
        (progn
        (increment-test-count)
        (format t "~%Test 'MOVE 5 :R1 (src littéral)' réussi.~%"))
        (format t "~%Test 'MOVE 5 :R1 (src littéral)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))

    ;; Cas où src est un registre
    (make-vm 'vm)
    (setf (get 'vm :R1) 10 (get 'vm :R2) 5)
    (vm-load 'vm '((MOVE :R2 :R1)))
    (vm-exec 'vm)
    (if (eq (get 'vm :R1) 5)
        (progn
        (increment-test-count)
        (format t "~%Test 'MOVE :R2 :R1 (src registre)' réussi.~%"))
        (format t "~%Test 'MOVE :R2 :R1 (src registre)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))
)
(defun test-exec-store ()
  (make-vm 'vm)
  (setf (get 'vm :R1) 42)
  (vm-load 'vm '((STORE :R1 10)))
  (vm-exec 'vm)
  (if (eq (get-from-vm-mem 'vm 10) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'STORE :R1 10' réussi.~%"))
      (format t "~%Test 'STORE :R1 10' échoué, valeur attendue : 42, obtenue : ~A.~%" (get-from-vm-mem 'vm 10))))

(defun test-exec-load ()
  (make-vm 'vm)
  (set-to-vm-mem 'vm 10 42)
  (vm-load 'vm '((LOAD 10 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 42)
      (progn
        (increment-test-count)
        (format t "~%Test 'LOAD 10 :R1' réussi.~%"))
      (format t "~%Test 'LOAD 10 :R1' échoué, valeur attendue : 42, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-incr ()
  (make-vm 'vm)
  (setf (get 'vm :R1) 0)
  (vm-load 'vm '((INCR :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 1)
      (progn
        (increment-test-count)
        (format t "~%Test 'INCR :R1' réussi.~%"))
      (format t "~%Test 'INCR :R1' échoué, valeur attendue : 1, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-decr ()
  (make-vm 'vm)
  (setf (get 'vm :R1) 1)
  (vm-load 'vm '((DECR :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 0)
      (progn
        (increment-test-count)
        (format t "~%Test 'DECR :R1' réussi.~%"))
      (format t "~%Test 'DECR :R1' échoué, valeur attendue : 0, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-add ()
  ;; Cas où src est un littéral
  (make-vm 'vm)
  (setf (get 'vm :R1) 10)
  (vm-load 'vm '((ADD 5 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 15)
      (progn
        (increment-test-count)
        (format t "~%Test 'ADD 5 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'ADD 5 :R1 (src littéral)' échoué, valeur attendue : 15, obtenue : ~A.~%" (get 'vm :R1)))
  
  ;; Cas où src est un registre
  (make-vm 'vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 5)
  (vm-load 'vm '((ADD :R2 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 15)
      (progn
        (increment-test-count)
        (format t "~%Test 'ADD :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'ADD :R2 :R1 (src registre)' échoué, valeur attendue : 15, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-sub ()
  ;; Cas où src est un littéral
  (make-vm 'vm)
  (setf (get 'vm :R1) 10)
  (vm-load 'vm '((SUB 3 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 7)
      (progn
        (increment-test-count)
        (format t "~%Test 'SUB 3 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'SUB 3 :R1 (src littéral)' échoué, valeur attendue : 7, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm 'vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 3)
  (vm-load 'vm '((SUB :R2 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 7)
      (progn
        (increment-test-count)
        (format t "~%Test 'SUB :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'SUB :R2 :R1 (src registre)' échoué, valeur attendue : 7, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-mul ()
  ;; Cas où src est un littéral
  (make-vm 'vm)
  (setf (get 'vm :R1) 4)
  (vm-load 'vm '((MUL 3 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 12)
      (progn
        (increment-test-count)
        (format t "~%Test 'MUL 3 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'MUL 3 :R1 (src littéral)' échoué, valeur attendue : 12, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm 'vm)
  (setf (get 'vm :R1) 4 (get 'vm :R2) 3)
  (vm-load 'vm '((MUL :R2 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 12)
      (progn
        (increment-test-count)
        (format t "~%Test 'MUL :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'MUL :R2 :R1 (src registre)' échoué, valeur attendue : 12, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-div ()
  ;; Cas où src est un littéral
  (make-vm 'vm)
  (setf (get 'vm :R1) 10)
  (vm-load 'vm '((DIV 2 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 5)
      (progn
        (increment-test-count)
        (format t "~%Test 'DIV 2 :R1 (src littéral)' réussi.~%"))
      (format t "~%Test 'DIV 2 :R1 (src littéral)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1)))

  ;; Cas où src est un registre
  (make-vm 'vm)
  (setf (get 'vm :R1) 10 (get 'vm :R2) 2)
  (vm-load 'vm '((DIV :R2 :R1)))
  (vm-exec 'vm)
  (if (eq (get 'vm :R1) 5)
      (progn
        (increment-test-count)
        (format t "~%Test 'DIV :R2 :R1 (src registre)' réussi.~%"))
      (format t "~%Test 'DIV :R2 :R1 (src registre)' échoué, valeur attendue : 5, obtenue : ~A.~%" (get 'vm :R1))))

(defun test-exec-jmp ()
  (make-vm 'vm)
  (setf (get 'vm :R1) 0)
  (vm-load 'vm '((JMP label1)(MOVE 1 :R1)(LABEL label1)))
  (vm-exec 'vm)
  (if (= (get 'vm :R1) 0)
      (progn
        (increment-test-count)
        (format t "~%Test JMP passé"))
      (format t "~%Test JMP échoué")))

(defun test-exec-jgt ()
    ;;Test que le saut s'effectue bien si le flag GT est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 1)

    (vm-load 'vm '((JGT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGT passé"))
        (format t "~%Test JGT échoué"))
    ;;Test que le saut ne s'effectue pas si le flag GT est à 0
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 0)

    (vm-load 'vm '((JGT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JGT passé"))
        (format t "~%Test JGT échoué"))
)


(defun test-exec-jge ()
    ;;Test que le saut s'effectue bien si le flag GT est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 1)

    (vm-load 'vm '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)

    (vm-load 'vm '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
    ;;Test que le saut ne s'effectue pas si les flags GT et EQ sont à 0
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :GT) 0)
    (setf (get 'vm :EQ) 0)

    (vm-load 'vm '((JGE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JGE passé"))
        (format t "~%Test JGE échoué"))
)

(defun test-exec-jlt ()
    ;;Test que le saut s'effectue bien si le flag LT est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 1)

    (vm-load 'vm '((JLT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLT passé"))
        (format t "~%Test JLT échoué"))
    ;;Test que le saut ne s'effectue pas si le flag LT est à 0
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 0)

    (vm-load 'vm '((JLT label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JLT passé"))
        (format t "~%Test JLT échoué"))
)
(defun test-exec-jle ()
    ;;Test que le saut s'effectue bien si le flag LT est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 1)

    (vm-load 'vm '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)

    (vm-load 'vm '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
    ;;Test que le saut ne s'effectue pas si les flags LT et EQ sont à 0
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :LT) 0)
    (setf (get 'vm :EQ) 0)

    (vm-load 'vm '((JLE label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JLE passé"))
        (format t "~%Test JLE échoué"))
)


(defun test-exec-jeq ()
    ;;Test que le saut s'effectue bien si le flag EQ est à 1
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 1)

    (vm-load 'vm '((JEQ label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 0)
        (progn
            (increment-test-count)
            (format t "~%Test JEQ passé"))
        (format t "~%Test JEQ échoué"))
    ;;Test que le saut ne s'effectue pas si le flag EQ est à 0
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (setf (get 'vm :EQ) 0)

    (vm-load 'vm '((JEQ label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (= (get 'vm :R1) 1)
        (progn
            (increment-test-count)
            (format t "~%Test JEQ passé"))
        (format t "~%Test JEQ échoué"))
)

(defun test-exec-jsr ()
    ;;Test que le saut s'effectue bien et que l'adresse de retour est sur la pile
    (make-vm 'vm)
    (setf (get 'vm :R1) 0)
    (vm-load 'vm '((JSR label1)(MOVE 1 :R1)(LABEL label1)))
    (vm-exec 'vm)
    (if (and (= (get 'vm :R1) 0) (= (get-from-vm-mem 'vm (- (get 'vm :SP) 1 )) (get 'vm :startCode)))
        (progn
            (increment-test-count)
            (format t "~%Test JSR passé"))
        (format t "~%Test JSR échoué")
     )
)

(defun test-exec-rtn ()
    (make-vm 'vm)
    (vm-load 'vm '((JMP finfonction1)(LABEL fonction1)(MOVE 1 :R1)(RTN)(LABEL inateignable)(MOVE 1 :R2)(LABEL finfonction1)(JSR fonction1)(MOVE 1 :R3)))
    (vm-exec 'vm)
    (vm-print 'vm)
    (if (and (and (= (get 'vm :R1) 1) (= (get 'vm :R2) 0)) (= (get 'vm :R3) 1) )
            (progn
                (increment-test-count)
                (format t "~%Test RTN passé"))
            (format t "~%Test RTN échoué")
    )
)