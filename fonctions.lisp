(defun incr-reg (vm reg)
  (setf(get vm reg) (+ 1 (get vm reg)))
)

(defun set-to-vm-mem (vm index val)
    "Set la valeur val en mémoire à un index précisé en paramètre"
    (setf (aref (get vm :mem) index) val)
)
(defun get-from-vm-mem (vm index)
    "Récupère la valeur en mémoire à un index précisé en paramètre"
    (aref (get vm :mem) index) 
)
(defun inst-move (vm src dest)
  "Copie la valeur de src (registre ou littéral) dans le registre dest"
  (let ((value (if (symbolp src) (get vm src) src))) 
    (setf(get vm dest) value)
  )
)
;;(trace inst-move)

(defun inst-store (vm reg index)
  "Stocke la valeur du registre reg à l'adresse index en mémoire"
  (set-to-vm-mem vm index (get vm reg))
)
;;(trace inst-store)
(defun inst-load (vm index reg)
  "Charge la valeur à l'adresse index de la mémoire vers le registre reg"
  (setf(get vm reg) (get-from-vm-mem vm index))
)

(defun inst-add (vm reg1 reg2)
  "Ajoute le premier registre au second et stocke le résultat dans le second"
  (setf (get vm reg2) (+ (get vm reg1) (get vm reg2)))
)