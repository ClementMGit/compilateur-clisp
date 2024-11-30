;; Fonctions générales
(defun incr-reg (vm reg)
  "Incrémente le registre reg de 1"
  (setf(get vm reg) (+ 1 (get vm reg)))
)
(defun set-to-vm-mem (vm index val)
    "Set la valeur val en mémoire à l'index précisé en paramètre"
    (setf (aref (get vm :mem) index) val)
)
(defun get-from-vm-mem (vm index)
    "Récupère la valeur en mémoire à l'index précisé en paramètre"
    (aref (get vm :mem) index) 
)
;; Fonctions d'exécution d'instruction assembleur
(defun exec-move (vm src dest)
  "Copie la valeur de src (registre ou littéral) dans le registre dest"
  (let ((source (if (symbolp src) (get vm src) src))) 
    (setf(get vm dest) source)
  )
)
(defun exec-store (vm reg index)
  "Stocke la valeur du registre reg à l'adresse index en mémoire"
  (set-to-vm-mem vm index (get vm reg))
)
(defun exec-load (vm index reg)
  "Charge la valeur à l'adresse index de la mémoire vers le registre reg"
  (setf(get vm reg) (get-from-vm-mem vm index))
)
(defun exec-add (vm src dest)
  "Addition telle que dest = dest+src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) src))) 
    (setf (get vm dest) (+ source (get vm dest)))
  )
)
(defun exec-sub (vm src dest)
  "Soustraction telle que dest = dest-src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) src))) 
    (setf (get vm dest) (- (get vm dest) source))
  )
)
(defun exec-mul (vm src dest)
  "Multiplication telle que dest = dest*src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) src))) 
    (setf (get vm dest) (* (get vm dest) source))
  )
)
(defun exec-div (vm src dest)
  "Division telle que dest = dest/src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) src))) 
    (setf (get vm dest) (/ (get vm dest) source))
  )
)