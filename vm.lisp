(require "debug.lisp")

(defun make-vm (&optional (vm 'vm) (size 1000))
  "Création d'une machine virtuelle"
  ;; On affecte a chaque propriéte/registre un nom et une valeur initiale
  (setf(get vm :nomvm) vm)
  (setf(get vm :size) size)
  (setf(get vm :RUNNING) nil)
  ;;Registres R0 à R3
  (setf(get vm :R0) 0)
  (setf(get vm :R1) 0)
  (setf(get vm :R2) 0)
  (setf(get vm :R3) 0)
  ;;Base pointer, début de la pile
  (setf(get vm :BP) 50)
  ;;Stack pointer, pointeur pile actuel
  (setf(get vm :SP) 50)
  ;;Frame pointer, pointe sur le nb arguments de la fonction empilé
  (setf(get vm :FP) 0)
  ;;Drapeaux Equal, plus grand, plus petit -> pour les cmp
  (setf(get vm :EQ) 0)
  (setf(get vm :GT) 0)
  (setf(get vm :LT) 0)
  ;;Max stack, fin de la pile, vers 75% de la taille de la mémoire
  (setf(get vm :maxStack) (floor (* size 0.75)))
  ;;Start Code, début de la zone de code juste après la fin de la pile
  (setf(get vm :startCode) (+ 1 (get vm :maxStack)))
  ;;Program counter, initialisé au début de la zone de code
  (setf(get vm :PC) (get vm :startCode))
  ;;Load Counter, initialisé au début de la zone de code
  (setf(get vm :LC) (get vm :startCode))
  (setf(get vm :mem) (make-array size))
  ;;Hash tables pour la résolution d'étiquettes
  (setf(get vm :knownLabels) (make-hash-table))
  (setf(get vm :unknownLabels) (make-hash-table))
  ;;Affichage de l'état initial de la VM
  (vm-print vm)
)
(defun vm-load (vm asm)
  (print "Chargement du code en mémoire...")
  (loop
    while (not (atom asm))
    do
    (format t "~%Chargement de ")
    (print (car asm))
    ;;On met l'instruction assembleur courante dans R0
    (exec-move vm (car asm) :R0)
    ;;On stocke l'instruction à l'adresse mémoire du LOAD COUNTER courant
    ;;Ce qui équivaut à dire on charge le code en mémoire
    (exec-store vm :R0 (get vm :LC))
    ;;Si le bout de code courant est un label
    (if (eq (car (get vm :R0)) 'LABEL)
    ;;On ajoute le label et son adresse+1 à la liste des labels connus
    (setf (gethash (second (get vm :R0)) (get vm :knownLabels)) (get vm :LC))
    ;;Sinon si on croise un JMP, JSR etc quelconque, idem pour les inconnus
    (if (char= (char (symbol-name (car (get vm :R0))) 0) #\J)
      (setf (gethash (second (get vm :R0)) (get vm :unknownLabels)) (get vm :LC))))
    ;;Dans tous les cas on incrémente Load counter et on passe a l'instruction assembleur suivante
    (exec-incr vm :LC)
    (setf asm (cdr asm))
  )
  ;;Résolution des étiquettes
  (resolve-jumps vm)
  (exec-move vm '(HALT) :R0)
  (exec-store vm :R0 (get vm :LC))  
  (vm-print vm)
  '(Code chargé en mémoire !)
)
(defun resolve-jumps (vm)
  "Résout les adresses de chaque JUMP non résolu"
  (maphash
    (lambda (label indexDuJump)
      (let ((known-address (gethash label (get vm :knownLabels))))
        (if known-address
            ;; Si le label est connu, remplacer par <adresse> dans le JMP/JSR/etc 
            (set-to-vm-mem vm indexDuJump (list (first (get-from-vm-mem vm indexdujump)) known-address))
            ;; Si le label est inconnu, remplacer par FUNCALL <label>
            (set-to-vm-mem vm indexDuJump (list `FUNCALL label)))))
    (get vm :unknownLabels)
  )
)
(defun exec-instr (vm instr)
  "Exécute l'instruction passée en paramètre"
  (format t "~%Exécution de ~S" instr)
  (format t "~%PC : ~D" (get vm :PC))
  (let ((arg1 (second instr)) 
       (arg2 (third instr)))
    (case (first instr)
      ('MOVE (exec-move vm arg1 arg2))
      ('STORE (exec-store vm arg1 arg2))
      ('LOAD (exec-load vm arg1 arg2))
      ('PUSH (exec-push vm arg1))
      ('POP (exec-pop vm arg1))
      ('INCR (exec-incr vm arg1))
      ('DECR (exec-decr vm arg1))
      ('ADD (exec-add vm arg1 arg2))
      ('SUB (exec-sub vm arg1 arg2))
      ('MUL (exec-mul vm arg1 arg2))
      ('DIV (exec-div vm arg1 arg2))
      ('RTN (exec-rtn vm))
      ('CMP (exec-cmp vm arg1 arg2))
      ('JMP (exec-jmp vm arg1))
      ('JSR (exec-jsr vm arg1))
      ('JLT (exec-jlt vm arg1))
      ('JLE (exec-jle vm arg1))
      ('JGT (exec-jgt vm arg1))
      ('JGE (exec-jge vm arg1))
      ('NOP ())
      ('HALT (setf(get vm :RUNNING) nil))
    )
  )
)
(defun vm-exec (&optional (vm 'vm))
  "Exécute le code présent en mémoire de la VM"
  (setf(get vm :RUNNING) T)
  ;; Tant que la vm tourne on execute les instructions
  (loop while (get vm :RUNNING) do 
    ;; On récup l'instruction courante via PC
    (let ((instr (get-from-vm-mem vm (get vm :PC)))) 
    ;;On exécute l'instruction courante
    (exec-instr vm instr)
    ;;On incrémente le PC
    (exec-incr vm :PC)
    )
  )
)