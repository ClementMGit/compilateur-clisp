
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

  (set-vm-registre vm :R0 0)
  (set-vm-registre vm :R1 0)
  (set-vm-registre vm :R2 0)
  (set-vm-registre vm :R3 0)
  ;;base pointer, début de la pile
  (set-vm-registre vm :BP 100)
  ;;stack pointer, pointeur pile actuel
  (set-vm-registre vm :SP 100)
  ;;frame pointer, pointe sur le nb arguments de la fonction empilé
  (set-vm-registre vm :FP 0)
  ;;Drapeaux Equal, plus grand, plus petit pour les cmp
  (set-vm-registre vm :EQ 0)
  (set-vm-registre vm :PG 0)
  (set-vm-registre vm :PP 0)

  (vm-init-memory vm size)
  (vm-print vm)
)
(trace make-vm)
;; ********** Vidage mémoire d'une machine virtuelle.

(defun vm-init-memory (&optional (vm 'vm) (memsize 1000))
  (set-vm-registre vm :PC 0)
  (set-vm-property vm :mem (make-array memsize))
)

(defun vm-exec-instr (vm instr)
  ;;TODO
)
;; ********** Chargement de code dans la mémoire d'une machine virtuelle.

(defun vm-load (vm asm)
  ;;TODO
)

;;Récupère la valeur en mémoire à un index précisé en paramètre
(defun get-from-vm-mem(vm index)
(aref (get vm :mem) index) 
)
;; ********** Exécution de la machine virtuelle.
  
(defun vm-exec(&optional (vm 'vm))
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
  (format t "~%- Compteurs : ~%--- PC : ~D"
	  (get-vm-registre vm :PC))
  (format t "~%RUNNING : ~S ~%" (get-vm-property vm :RUNNING))
  (format t "~%Mémoire :~% ~% ~S ~%" (get-vm-property vm :mem))
  )
