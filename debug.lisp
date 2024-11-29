(require "fonctions.lisp")
(defun vm-print (&optional (vm 'vm))
    "Affiche tous les paramètres d'une machine virtuelle"
    (format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille : ~D" (get-vm-property vm :nomvm)(get-vm-property vm :size))
    (format t "~%- Registres : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D ~%--- R3 : ~D" (get-vm-registre vm :R0) (get-vm-registre vm :R1) (get-vm-registre vm :R2) (get-vm-registre vm :R3))
    (format t "~%- Pointeurs : ~%--- BP : ~D ~%--- SP : ~D ~%--- FP : ~D" (get-vm-registre vm :BP) (get-vm-registre vm :SP) (get-vm-registre vm :FP))
    (format t "~%- Drapeaux : ~%--- EQ : ~D ~%--- PP : ~D ~%--- PG : ~D" (get-vm-registre vm :EQ) (get-vm-registre vm :PP) (get-vm-registre vm :PG))
    (format t "~%- Compteurs : ~%--- PC : ~D ~%--- LC : ~D" (get-vm-registre vm :PC) (get-vm-registre vm :LC))
    (format t "~%RUNNING : ~S ~%" (get-vm-property vm :RUNNING))
    (format t "~%Mémoire :~% ~% ~S ~%" (get-vm-property vm :mem))
)

(defun print-hash-table (hash-table)
    "Affiche le contenu d'une hash table passée en paramètre"
  (format t "~%TABLE DE HASH ~%")
  (maphash (lambda (key value)
             (format t "Clé: ~A, Valeur: ~A~%" key value))
           hash-table)
)