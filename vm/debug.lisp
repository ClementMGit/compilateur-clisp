(require "fonctions.lisp")
(defun vm-print (&optional (vm 'vm))
    "Affiche tous les paramètres d'une machine virtuelle"
    (format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille : ~D" (get vm :nomvm)(get vm :size))
    (format t "~%- Registres : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D ~%--- R3 : ~D" (get vm :R0) (get vm :R1) (get vm :R2) (get vm :R3))
    (format t "~%- Pointeurs : ~%--- BP : ~D ~%--- SP : ~D ~%--- FP : ~D" (get vm :BP) (get vm :SP) (get vm :FP))
    (format t "~%- Drapeaux : ~%--- EQ : ~D ~%--- LT : ~D ~%--- GT : ~D" (get vm :EQ) (get vm :LT) (get vm :GT))
    (format t "~%- Compteurs : ~%--- PC : ~D ~%--- LC : ~D" (get vm :PC) (get vm :LC))
    (format t "~%Mémoire :~% ~% ~S ~%" (get vm :mem))
)

(defun print-hash-table (hash-table)
    "Affiche le contenu d'une hash table passée en paramètre"
  (format t "~%TABLE DE HASH ~%")
  (maphash (lambda (key value)
             (format t "Clé: ~A, Valeur: ~A~%" key value))
           hash-table)
)