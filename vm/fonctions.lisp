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
  (let ((source (cond ((islitteral src) (cadr src)) ((symbolp src) (get vm src)) (t src)))) 
    (setf(get vm dest) source)
  )
)
(defun isLitteral (arg)
  "Vérifie que l'argument est une liste et que son premier élément est 'LIT'"
    (and (consp arg) (eql (car arg) 'LIT))
)
(defun exec-store (vm src dest)
  "Stocke la valeur du registre ou littéral src à l'adresse spécifiée par dest en mémoire.
   Par exemple : (STORE :R0 :FP) ou (STORE :R0 (:FP -3))."
  (let ((destination (cond
                       ;; Si dest est un registre, récupérer son contenu comme adresse
                       ((symbolp dest) (get vm dest))
                       ;; Sinon, c'est une adresse littérale ou autre valeur directe
                       ((islitteral dest) (cadr dest))
                        ;; Si dest est une liste du type `(:registre offset)`
                       ((and (listp dest) (= (length dest) 2) (symbolp (first dest)) (numberp (second dest)))
                        (+ (get vm (first dest)) (second dest)))
                       (t dest)))
        (value (cond ((symbolp src) (get vm src)) ((islitteral src) (cadr src)) (t src)))) ; Récupérer la valeur de src (registre ou littéral)
    ;; Écrire la valeur dans la mémoire à l'adresse calculée
    (set-to-vm-mem vm destination value))
)

(defun exec-load (vm src dest)
  "Charge la valeur à l'adresse (registre ou littéral) src de la mémoire vers le registre dest.
   Par exemple : (LOAD :FP :R0) ou (LOAD (:FP -3) :R0)."
  (let ((source (cond
                  ;; Si src est un registre, récupérer sa valeur
                  ((symbolp src) (get vm src))
                  ;; Sinon, c'est une adresse littérale ou autre valeur directe
                  ((islitteral src) (cadr src))
                  ;; Si src est une liste du type `(:registre offset)`
                  ((and (listp src) (= (length src) 2) (symbolp (first src)) (numberp (second src)))
                  (+ (get vm (first src)) (second src)))

                  (t src))))
    ;; Charger la valeur de la mémoire et la placer dans dest
    (setf (get vm dest) (get-from-vm-mem vm source)))
)

(defun exec-incr (vm reg)
  "Incrémente le registre reg de 1"
  (setf(get vm reg) (+ 1 (get vm reg)))
)
(defun exec-decr (vm reg)
  "Décrémente le registre reg de 1"
  (setf(get vm reg) (- (get vm reg) 1))
)
(defun exec-add (vm src dest)
  "Addition telle que dest = dest+src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) (cadr src)))) 
    (setf (get vm dest) (+ source (get vm dest)))
  )
)
(defun exec-sub (vm src dest)
  "Soustraction telle que dest = dest-src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) (cadr src)))) 
    (setf (get vm dest) (- (get vm dest) source))
  )
)
(defun exec-mul (vm src dest)
  "Multiplication telle que dest = dest*src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) (cadr src)))) 
    (setf (get vm dest) (* (get vm dest) source))
  )
)
(defun exec-div (vm src dest)
  "Division telle que dest = dest/src où src peut être un littéral ou un registre"
  (let ((source (if (symbolp src) (get vm src) (cadr src)))) 
    (setf (get vm dest) (/ (get vm dest) source))
  )
)
(defun exec-jmp (vm indexToJumpTo)
  "Saut inconditionnel"
  (setf(get vm :PC) indexToJumpTo)
)
(defun exec-jgt (vm indexToJumpTo)
  "Saut si drapeau :GT = 1"
  (if (= (get vm :GT) 1)
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jge (vm indexToJumpTo)
  "Saut si drapeau :GT = 1 ou :EQ = 1"
  (if (or (= (get vm :GT) 1) (= (get vm :EQ) 1))
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jlt (vm indexToJumpTo)
  "Saut si drapeau :LT = 1"
  (if (= (get vm :LT) 1)
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jle (vm indexToJumpTo)
  "Saut si drapeau :LT = 1 ou :EQ = 1"
  (if (or (= (get vm :LT) 1) (= (get vm :EQ) 1))
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jeq (vm indexToJumpTo)
  "Saut si drapeau :EQ = 1"
  (if (= (get vm :EQ) 1)
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jne (vm indexToJumpTo)
  "Saut si drapeau :EQ = 0"
  (if (= (get vm :EQ) 0)
    (setf(get vm :PC) indexToJumpTo))
)
(defun exec-jsr (vm indexToJumpTo)
  "Saut vers une fonction connue"
  ;;On sauvegarde l'adresse de retour :PC sur la pile
  (set-to-vm-mem vm (get vm :SP) (get vm :PC))
  ;;On incrémente :SP
  (exec-incr vm :SP)
  ;;On saute vers la fonction
  (setf(get vm :PC) indexToJumpTo)
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
  (let ((arg1 (if (symbolp firstarg) (get vm firstarg) (cadr firstarg)))
        (arg2 (if (symbolp secondarg) (get vm secondarg) (cadr secondarg))))
    (if (= arg1 arg2)
      (progn (setf(get vm :EQ) 1) (setf(get vm :LT) 0) (setf(get vm :GT) 0))
      (if (< arg1 arg2)
        (progn (setf(get vm :EQ) 0) (setf(get vm :LT) 1) (setf(get vm :GT) 0))
        (progn (setf(get vm :EQ) 0) (setf(get vm :LT) 0) (setf(get vm :GT) 1))
      )
    )
  ) 
)
(defun exec-push (vm src)
  "Ajoute src, un ittéral ou registre au sommet de la pile"
  (let ((source (if (symbolp src) (get vm src) (cadr src))))
    (if (= (get vm :SP) (get vm :maxStack))
      (error "Stack overflow !")
      (progn 
        (set-to-vm-mem vm (get vm :SP) source)
        (exec-incr vm :SP)
      )
    )
  )
)
(defun exec-pop (vm dest)
  "Copie le sommet de la pile dans le registre dest"
  (if (= (get vm :SP) (get vm :BP))
    (error "Pile vide, rien à retirer !")
    (progn 
      (exec-decr vm :SP)
      (setf(get vm dest) (get-from-vm-mem vm (get vm :SP)))
    )
  )
)
(defun exec-funcall (vm args)
  "Exécute une fonction Lisp avec les arguments présents sur la pile."
  (let* ((func (first args)); Fonction à appeler
         (nb-args (get-from-vm-mem vm (get vm :FP))); Nombre d'arguments
         (resolved-args '())); Liste des arguments à résoudre
    ;; Construire la liste des arguments depuis la pile
    (let ((i 1))
      (loop while (<= i nb-args) do
            (push (get-from-vm-mem vm (- (get vm :FP) i)) resolved-args)
            (incf i)
      )
    )  
    ;; Appeler la fonction avec les arguments résolus et Stocker le résultat  dans R0
    (setf (get vm :R0) (apply func (reverse resolved-args)))
  )
)
