;; Fonctions générales
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
(defun exec-incr (vm reg)
  "Incrémente le registre reg de 1"
  (setf(get vm reg) (+ 1 (get vm reg)))
)
(defun exec-decr (vm reg)
  "Décrémente le registre reg de 1"
  (setf(get vm reg) (- 1 (get vm reg)))
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
(defun exec-jmp (vm indexToJumpTo)
  "Saut inconditionnel"
  (setf(get vm :PC) indextojumpto)
)
(defun exec-jgt (vm indexToJumpTo)
  "Saut si drapeau :GT = 1"
  (if (= (get vm :GT) 1)
    (setf(get vm :PC) indextojumpto))
)
(defun exec-jge (vm indexToJumpTo)
  "Saut si drapeau :GT = 1 ou :EQ = 1"
  (if (or (= (get vm :GT) 1) (= (get vm :EQ) 1))
    (setf(get vm :PC) indextojumpto))
)
(defun exec-jlt (vm indexToJumpTo)
  "Saut si drapeau :LT = 1"
  (if (= (get vm :LT) 1)
    (setf(get vm :PC) indextojumpto))
)
(defun exec-jle (vm indexToJumpTo)
  "Saut si drapeau :LT = 1 ou :EQ = 1"
  (if (or (= (get vm :LT) 1) (= (get vm :EQ) 1))
    (setf(get vm :PC) indextojumpto))
)
(defun exec-jeq (vm indexToJumpTo)
  "Saut si drapeau :EQ = 1"
  (if (= (get vm :EQ) 1)
    (setf(get vm :PC) indextojumpto))
)
(defun exec-jsr (vm indexToJumpTo)
  "Saut vers une fonction connue"
  ;;On sauvegarde l'adresse de retour :PC sur la pile
  (set-to-vm-mem vm (get vm :SP) (get vm :PC))
  ;;On incrémente :SP
  (exec-incr vm :SP)
  ;;On saute vers la fonction
  (setf(get vm :PC) indextojumpto)
)
(defun exec-rtn (vm)
  "Retourne d'une fonction vers l'appelant"
  ;;On décrémente :SP
  (exec-decr vm :SP)
  ;;On saute à l'adresse stocké dans :SP
  (setf(get vm :PC) (get-from-vm-mem vm (get vm :SP)))
)
(defun exec-cmp (vm firstarg secondarg)
  "Compare arg1 et arg2 registres ou littéraux et met à jour les flags en conséquence"
  (let ((arg1 (if (symbolp firstarg) (get vm firstarg) firstarg))
        (arg2 (if (symbolp secondarg) (get vm secondarg) secondarg)))
    (if (equal arg1 arg2)
      (progn ((setf(get vm :FEQ) 1)) ((setf(get vm :FLT) 0)) ((setf(get vm :FGT) 0)))
      (if (< arg1 arg2)
        (progn ((setf(get vm :FEQ) 0)) ((setf(get vm :FLT) 1)) ((setf(get vm :FGT) 0)))
        (progn ((setf(get vm :FEQ) 0)) ((setf(get vm :FLT) 0)) ((setf(get vm :FGT) 1)))
      )
    )
  ) 
)
(defun exec-push (vm arg1)
  "Ajoute argl, un ittéral ou registre au sommet de la pile"
  (let ((arg (if (symbolp arg1) (get vm arg1) arg1)))
    (if (= (get vm :SP) (get vm :maxStack))
      (error "Stack overflow !")
      (progn 
        (set-to-vm-mem vm (get vm :SP) arg)
        (exec-incr vm :SP)
      )
    )
  )
)
(defun exec-pop (vm reg)
  "Ajoute argl, un ittéral ou registre au sommet de la pile"
  (if (= (get vm :SP) (get vm :BP))
    (error "Pile vide, rien à retirer !")
    (progn 
      (exec-decr vm :SP)
      (setf(get vm reg) (get-from-vm-mem vm (get vm :SP)))
    )
  )
)