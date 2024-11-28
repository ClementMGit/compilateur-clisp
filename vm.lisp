

(defun get-vm-registre (vm reg)
	(get vm reg)
)
(defun set-vm-registre (vm reg val)
	(setf(get vm reg) val)
)
(defun incr-vm-registre (vm reg)
  (setf(get vm reg) (+ 1 (get vm reg)))
)

(defun get-vm-property (vm reg)
	(get vm reg)
)
(defun set-vm-property (vm reg val)
	(setf(get vm reg) val)
)

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

  (set-vm-property vm :etiq (make-hash-table))
  (set-vm-property vm :etiqNR (make-hash-table))

  ;;Affichage de l'état initial de la VM
  (vm-print vm)

)

;;(trace make-vm)

(defun vm-exec-instr (vm instr)
  ;;TODO
)

;; ********** Chargement de code dans la mémoire d'une machine virtuelle.

(defun vm-load (vm asm)
  (print "~~~~~~Chargement du code en mémoire...~~~~~~")

  (loop
    while (not (atom asm))
    do
    (progn
      (print (car asm))
      ;;On met (car asm) dans le registre R0
      (inst-move vm (car asm) :R0)
      ;;On stocke la valeur du registre R0 à l'adresse mémoire du LOAD COUNTER courant
      ;;Ce qui équivaut a dire on charge le code en mémoire
      (inst-store vm :R0 (get-vm-registre vm :LC))
      ;;Si le bout de code courant est un label
      (if (eq (car (get-vm-registre vm :R0)) 'LABEL)
      ;;On appel une sous fonction avec en argument le nom de l'étiquette
      ;;qui gère la résolution d'adresse
        'TODO
        ;;(vm_label vm (cdr (get-vm-registre vm :R0)))
      )
      (incr-vm-registre vm :LC)
      (setf asm (cdr asm))
    )
  )
  (inst-move vm '(HALT) :R0)
  (inst-store vm :R0 (get-vm-registre vm :LC))
  ;;(incr-vm-registre vm :LC)
)
(trace vm-load)

;;Récupère la valeur en mémoire à un index précisé en paramètre
(defun set-to-vm-mem (vm index val)
  (setf (aref (get vm :mem) index) val)
)
;;Récupère la valeur en mémoire à un index précisé en paramètre
(defun get-from-vm-mem (vm index)
  (aref (get vm :mem) index) 
)
(defun inst-move (vm src dest)
  "Copie la valeur de src (registre ou littéral) dans le registre dest"
  (let ((value (if (symbolp src) (get-vm-registre vm src) src))) 
    (set-vm-registre vm dest value)
  )
)
(trace inst-move)

(defun inst-store (vm reg index)
  "Stocke la valeur du registre reg à l'adresse index en mémoire"
  (set-to-vm-mem vm index (get-vm-registre vm reg))
)
(trace inst-store)
(defun inst-load (vm index reg)
  "Charge la valeur à l'adresse index de la mémoire vers le registre reg"
  (set-vm-registre vm reg (get-from-vm-mem vm index)))
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


;; ********** Affichage de tous les paramètres d'une machine virtuelle.

(defun vm-print (&optional (vm 'vm))
  (format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille : ~D" (get-vm-property vm :nomvm)(get-vm-property vm :size))
  (format t "~%- Registres : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D ~%--- R3 : ~D" 
	  (get-vm-registre vm :R0) (get-vm-registre vm :R1) (get-vm-registre vm :R2) (get-vm-registre vm :R3))
  (format t "~%- Pointeurs : ~%--- BP : ~D ~%--- SP : ~D ~%--- FP : ~D"
	  (get-vm-registre vm :BP) (get-vm-registre vm :SP) (get-vm-registre vm :FP))
  (format t "~%- Drapeaux : ~%--- EQ : ~D ~%--- PP : ~D ~%--- PG : ~D"
	  (get-vm-registre vm :EQ) (get-vm-registre vm :PP) (get-vm-registre vm :PG))
  (format t "~%- Compteurs : ~%--- PC : ~D ~%--- LC : ~D"
	  (get-vm-registre vm :PC) (get-vm-registre vm :LC))
  (format t "~%RUNNING : ~S ~%" (get-vm-property vm :RUNNING))
  (format t "~%Mémoire :~% ~% ~S ~%" (get-vm-property vm :mem))
  )
