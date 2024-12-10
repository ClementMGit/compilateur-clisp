(require "fonctions.lisp")
(defun vm-print (&optional (vm 'vm))
    "Affiche tous les paramètres d'une machine virtuelle"
    (format t "~%~%Machine virtuelle :        Registres :      Pointeurs :      Drapeaux :      Compteurs : ~%Nom : ~S                   R0 : ~D          BP : ~D          EQ : ~D          PC : ~D~%Taille : ~D               R1 : ~D          SP : ~D          LT : ~D          LC : ~D~%                           R2 : ~D          FP : ~D          GT : ~D~%                           R3 : ~D "(get vm :nomvm) (get vm :R0) (get vm :BP) (get vm :EQ) (get vm :PC)(get vm :size) (get vm :R1) (get vm :SP) (get vm :LT)  (get vm :LC) (get vm :R2)  (get vm :FP)  (get vm :GT) (get vm :R3))
    (format t "~%Mémoire : ~S ~%" (get vm :mem))
)

(defun print-hash-table (hash-table)
    "Affiche le contenu d'une hash table passée en paramètre"
  (format t "~%TABLE DE HASH ~%")
  (maphash (lambda (key value)
             (format t "Clé: ~A, Valeur: ~A~%" key value))
           hash-table)
)