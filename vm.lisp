(require "debug.lisp")

;; ********** Création d'une machine virtuelle.

(defun make-vm (&optional (vm 'vm) (size 1000))
;; On affecte a chaque propriéte/registre un nom et une valeur
  (setf(get vm :nomvm) vm)
  (setf(get vm :size) size)
  (setf(get vm :RUNNING) nil)
  ;;Registres R0 à R3
  (setf(get vm :R0) 0)
  (setf(get vm :R1) 0)
  (setf(get vm :R2) 0)
  (setf(get vm :R3) 0)
  ;;base pointer, début de la pile
  (setf(get vm :BP) 50)
  ;;stack pointer, pointeur pile actuel
  (setf(get vm :SP) 50)
  ;;frame pointer, pointe sur le nb arguments de la fonction empilé
  (setf(get vm :FP) 0)
  ;;Drapeaux Equal, plus grand, plus petit pour les cmp
  (setf(get vm :EQ) 0)
  (setf(get vm :PG) 0)
  (setf(get vm :PP) 0)
  ;;Max stack, fin de la pile, vers 75% de la taille de la mémoire
  (setf(get vm :maxStack) (floor (* size 0.75)))
  ;;Start Code, début de la zone de code juste après la fin de la pile
  (setf(get vm :startCode) (+ 1 (get vm :maxStack)))
  ;;Program counter, initialisé au début de la zone de code
  (setf(get vm :PC) (get vm :startCode))
  ;;Load Counter, initialisé au début de la zone de code
  (setf(get vm :LC) (get vm :startCode))
  (setf(get vm :mem) (make-array size))
  ;;Hash table pour la résolution d'étiquettes
  (setf(get vm :knownLabels) (make-hash-table))
  (setf(get vm :unknownLabels) (make-hash-table))
  ;;Affichage de l'état initial de la VM
  (vm-print vm)
)



;; ********** Chargement de code dans la mémoire d'une machine virtuelle.

(defun vm-load (vm asm)
  (print "~~~~~~Chargement du code en mémoire...~~~~~~")
  (loop
    while (not (atom asm))
    do
    (format t "~%Chargement de ")
    (print (car asm))
    ;;On met (car asm) dans le registre R0
    (exec-move vm (car asm) :R0)
    ;;On stocke la valeur du registre R0 à l'adresse mémoire du LOAD COUNTER courant
    ;;Ce qui équivaut a dire on charge le code en mémoire
    (exec-store vm :R0 (get vm :LC))
    ;;Si le bout de code courant est un label
    (if (eq (car (get vm :R0)) 'LABEL)
    ;;On ajoute le label et son adresse+1 à la liste des labels connus
    (setf (gethash (second (get vm :R0)) (get vm :knownLabels)) (+ 1 (get vm :LC)))
    ;;Sinon si on croise un JUMP quelconque TODO, idem pour les inconnus
    (if (eq (car (get vm :R0)) 'JUMP)
      (setf (gethash (second (get vm :R0)) (get vm :unknownLabels)) (get vm :LC))))
    ;;Dans tous les cas on incrément load counter et on passe a l'instruction assembleur suivante
    (incr-reg vm :LC)
    (setf asm (cdr asm))
  )
  (resolve-jumps vm)
  (exec-move vm '(HALT) :R0)
  (exec-store vm :R0 (get vm :LC))  
  (vm-print vm)
  '(Code chargé en mémoire !)
)


(defun resolve-jumps (vm)
  "On résoud chaque JUMP non résolu"
  (maphash
    (lambda (label indexDuJump)
      (let ((known-address (gethash label (get vm :knownLabels))))
        (if known-address
            ;; Si le label est connu, remplacer par JUMP <adresse>
            (set-to-vm-mem vm indexDuJump (list `JUMP known-address))
            ;; Si le label est inconnu, remplacer par FUNCALL <label>
            (set-to-vm-mem vm indexDuJump (list `FUNCALL label)))))
    (get vm :unknownLabels)
  )
)
;; ********** Exécution de la machine virtuelle.
  
(defun exec-instr (vm instr)
  (format t "~%Exécution de ~S" instr)
  (format t "~%PC : ~D" (get vm :PC))
  (case (first instr)
    ('MOVE (exec-move vm (second instr) (third instr)))
    ('ADD (exec-add vm (second instr) (third instr)))
    ('SUB (exec-sub vm (second instr) (third instr)))
    ('MUL (exec-mul vm (second instr) (third instr)))
    ('DIV (exec-div vm (second instr) (third instr)))
    ('HALT (setf(get vm :RUNNING) nil))
  )
)
(defun vm-exec (&optional (vm 'vm))
  (setf(get vm :RUNNING) T)
  ;; Tant que la vm tourne on execute les instructions
  (loop while (get vm :RUNNING) do 
    ;; On récup l'instruction courante via PC
    (let ((instr (get-from-vm-mem vm (get vm :PC)))) 
    ;;On exécute l'instruction courante
    (exec-instr vm instr)
    (incr-reg vm :PC);;On incrémente le PC
    )
  )
)