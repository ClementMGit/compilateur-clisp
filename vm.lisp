(require "debug.lisp")

;; ********** Création d'une machine virtuelle.

(defun make-vm (&optional (vm 'vm) (size 1000))
;; On affecte a chaque propriéte/registre un nom et une valeur
  (set-vm-property vm :nomvm vm)
  (set-vm-property vm :size size)
  (set-vm-property vm :RUNNING nil)
  ;;Registres R0 à R3
  (set-vm-registre vm :R0 0)
  (set-vm-registre vm :R1 0)
  (set-vm-registre vm :R2 0)
  (set-vm-registre vm :R3 0)
  ;;base pointer, début de la pile
  (set-vm-registre vm :BP 50)
  ;;stack pointer, pointeur pile actuel
  (set-vm-registre vm :SP 50)
  ;;frame pointer, pointe sur le nb arguments de la fonction empilé
  (set-vm-registre vm :FP 0)
  ;;Drapeaux Equal, plus grand, plus petit pour les cmp
  (set-vm-registre vm :EQ 0)
  (set-vm-registre vm :PG 0)
  (set-vm-registre vm :PP 0)
  ;;Max stack, fin de la pile
  (set-vm-property vm :maxStack (floor (* size 0.75)))
  ;;Start Code, début de la zone de code juste après la fin de la pile
  (set-vm-property vm :startCode (+ 1 (get-vm-property vm :maxStack)))
  ;;Program counter, initialisé au début de la zone de code
  (set-vm-registre vm :PC (get-vm-property vm :startCode))
  ;;Load Counter, initialisé au début de la zone de code
  (set-vm-registre vm :LC (get-vm-property vm :startCode))
  (set-vm-property vm :mem (make-array size))
  ;;Hash table pour la résolution d'étiquettes
  (set-vm-property vm :knownLabels (make-hash-table))
  (set-vm-property vm :unknownLabels (make-hash-table))
  ;;Affichage de l'état initial de la VM
  (vm-print vm)
)


(defun vm-exec-instr (vm instr)
  ;;TODO
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
    (inst-move vm (car asm) :R0)
    ;;On stocke la valeur du registre R0 à l'adresse mémoire du LOAD COUNTER courant
    ;;Ce qui équivaut a dire on charge le code en mémoire
    (inst-store vm :R0 (get-vm-registre vm :LC))
    ;;Si le bout de code courant est un label
    (if (eq (car (get-vm-registre vm :R0)) 'LABEL)
    ;;On ajoute le label et son adresse à la liste des labels connus
    (setf (gethash (second (get-vm-registre vm :R0)) (get-vm-property vm :knownLabels)) (get-vm-registre vm :LC))
    ;;Sinon si on croise un JUMP, idem pour les inconnus
    (if (eq (car (get-vm-registre vm :R0)) 'JUMP)
      (setf (gethash (second (get-vm-registre vm :R0)) (get-vm-property vm :unknownLabels)) (get-vm-registre vm :LC))))
    ;;Dans tous les cas on incrément load counter et on passe a l'instruction assembleur suivante
    (incr-vm-registre vm :LC)
    (setf asm (cdr asm))
  )
  (resolve-jumps vm)
  (inst-move vm '(HALT) :R0)
  (inst-store vm :R0 (get-vm-registre vm :LC))  
  (vm-print vm)
  '(Code chargé en mémoire !)
)


(defun resolve-jumps (vm)
  "On résoud chaque JUMP non résolu"
  (maphash
    (lambda (label indexDuJump)
      (let ((known-address (gethash label (get-vm-property vm :knownLabels))))
        (if known-address
            ;; Si le label est connu, remplacer par JUMP <adresse>
            (set-to-vm-mem vm indexDuJump (list `JUMP known-address))
            ;; Si le label est inconnu, remplacer par FUNCALL <label>
            (set-to-vm-mem vm indexDuJump (list `FUNCALL label)))))
    (get-vm-property vm :unknownLabels)
  )
)
;; ********** Exécution de la machine virtuelle.
  
(defun vm-exec (&optional (vm 'vm))
  (set-vm-registre vm :RUNNING T)
    ;; Tant que la vm tourne on execute les instructions
    (loop while (get-vm-registre vm :RUNNING) do 
      ;; On récup l'instruction courante via PC
      (let ((instr (get-from-vm-mem vm (get-vm-registre vm :PC)))) 

      ;;On exécute l'instruction courante
      (vm-exec-instr vm instr)
      (incr-vm-registre vm :PC);;On incrémente le PC
    )
  )
)