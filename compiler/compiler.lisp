(defun compilation (expr env)
  (if (atom expr)
    (compile-lit expr env)
    (let ((first-symbol (car expr)))
      (cond
      ((eql first-symbol 'let) (compile-let expr env))
      ((eql first-symbol 'if) (compile-if expr env))
      ((eql first-symbol 'loop) (compile-while expr env))
      ((eql first-symbol 'setq) (compile-setq expr env))
      ((eql first-symbol 'cond) (compile-cond (cdr expr) (gensym "fincond") env))
      ((eql first-symbol 'defun) (compile-defun expr env))
      ((eql first-symbol 'progn) (compile-progn (cdr expr) env))
      ((member first-symbol '(< > = <= >= )) (compile-comp expr env))
      ((member first-symbol '(+ - * /)) (compile-op expr env))
      (t (compile-fcall expr env))
      )
    )
  )
)
(defun compile-defun (expr env)
  "Compile une définition de fonction de la forme (defun nomfonction (paramètres) corps)"
  (let ((positionPile 0)
        (etiq-fin (gensym "findefun")))
    (dolist (param (third expr)) ;; Itère sur chaque paramètre.
    (setf positionPile (+ positionPile 1)) ;; Incrémente la position.
    (setf env (acons param (- 0 positionPile) env));; Met à jour `env`.
    ) 
    (append 
    `((JMP ,etiq-fin));C'est une déclaration de fonction, on ne doit pas exécuter son contenu sans passer par un jsr
    `((LABEL ,(second expr)));Label nomfonction
    (compilation (fourth expr) env);Compilation du corps de la fonction
    '((RTN))
    `((LABEL ,etiq-fin))
    
    )
  )
)
(defun compile-fcall (expr env)
  "Compile un appel de fonction de la forme (nomfonction arg1..argn)"
  (let ((nom-fonction (car expr))      ;; Le nom de la fonction appelée.
        (arguments (cdr expr))         ;; Les arguments de l'appel de fonction.
        (nb-arguments (length (cdr expr)))) ;; Le nombre d'arguments.
    (format t "~% Compilation de l'appel de la fonction ~S ~%" nom-fonction)
    (format t "~% cdr expr ~S ~%" (cdr expr))

    (append
     ;; Compilation des arguments et leur empilement.
    (apply 'append 
      (map 'list
      (lambda (arg)
        (append
         (compilation arg env); Compile chaque argument.
         '((PUSH :R0)); Empile sa valeur
        )
      )
      (reverse arguments))
    )
    `((PUSH ,nb-arguments)); Empile le nombre d'arguments.
      (format t "~% Compilation de l'appel de la fonction ~S ~%" (car expr))
    '((MOVE :FP :R1)); Sauvegarde old FP 
    '((MOVE :SP :FP));nouveau FP doit pointer sur nb args
    '((SUB 1 :FP))
    ;; Calcul de l'ancien SP.
    `((MOVE :SP :R2))
    `((SUB ,(+ nb-arguments 1) :R2))
    '((PUSH :R2)); Empile l'ancien SP.
    '((PUSH :R1)); Sauvegarde old FP

    `((JSR ,nom-fonction)); Appel de la fonction.
    
    ;; Restauration de l'état de la pile.
    '((POP :R1)); Récupère l'ancien FP.
    '((POP :R2)); Récupère l'ancien SP.
    '((MOVE :R1 :FP)); Restaure le FP.
    '((MOVE :R2 :SP)); Restaure le SP.
    )
  )
)
(defun compile-cond (expr etiq-fin env)
  "Compile un cond de la forme (cond ((test1) (expr1)) ((test2) (expr2)) )"
  (if (null expr)
    `((LABEL ,etiq-fin));Fin du cond 
    (let ((etiq-cond (gensym "cond")))                              
        (append (
          compilation (caar expr) env);Compilation de la condition             
          '((CMP :R0 0))                                
          `((JEQ ,etiq-cond))                             
          (compilation (cadar expr) env)              
          `((JMP ,etiq-fin))                               
          `((LABEL ,etiq-cond))                                    
          (compile-cond (cdr expr) etiq-fin env))
    )
  )
)
(defun compile-progn (expr env)
  (if (null expr) 
    ()
    (append (compilation (car expr) env) (compile-progn (cdr expr) env))
  )
)
(defun compile-setq (expr env)
  "Compile une expression setq de la forme (setq variable nouvelle-valeur)"
  (let ((variable-local (assoc (second expr) env)))  ; Recherche de la variable dans l'environnement
    (if variable-local  ; Si la variable est déjà locale
        (append 
         `((MOVE :FP :R1))                      ; Charge le pointeur de cadre (FP) dans R1.
         `((ADD ,(cdr variable-local) :R1))      ; Déduit la position de la variable dans la pile.
         '((PUSH :R1))
         (compilation (third expr) env)          ; Compiler la valeur à affecter
         '((POP :R1))
         '((STORE :R0 :R1)))                     ; Déplacer la valeur dans la variable locale
      (progn
        (format t "var globale, cas non traité pour l'instant")
        ;; Si la variable n'est pas trouvée, ajoute-la à l'environnement local
        (setq env (cons (cons (second expr) (length env)) env))  ; Ajouter la variable à l'environnement
        (append 
         `((MOVE :FP :R1))                      ; Charge le pointeur de cadre (FP) dans R1.
         `((ADD ,(length env) :R1))  ; Déduit la position de la variable dans la pile
          '((PUSH :R1))
         (compilation (third expr) env)          ; Compiler la valeur à affecter
         '((POP :R1))

         '((STORE :R0 :R1)))                     ; Déplacer la valeur dans la variable locale
      )
    ) 
  )
)


(defun compile-let (expr env)
  "Compile une expression LET, en gérant les variables locales et leur environnement."
  (let* ((bindings (second expr))        ; Les bindings locaux dans le let
         (updated-env env)            ; Environnement mis à jour après ajout des variables locales
         (setup-code '())             ; Code pour initialiser les variables locales
         (teardown-code '())         ; Code pour nettoyer après le let
         (i 0))
    ;; Initialisation des variables locales et mise à jour de l'environnement
    (loop while (< i (length bindings)) do
      (let* ((binding (nth i bindings))     ; Chaque binding
             (var-name (first binding))      ; Nom de la variable
             (var-init (second binding)))    ; Valeur initiale de la variable
        ;On ajoute a l'env une nouvelle association (nom_de_variable decalage_dans_la_pile)
        (setq updated-env (cons (cons var-name (length updated-env)) updated-env))
        ;; Compiler la valeur initiale et empiler
        (setq setup-code
              (append setup-code
                      (compilation var-init env)
                      '((PUSH :R0))))
      )
      (setq i (+ i 1)); Incrémenter l'indice
    )  
    ;; Générer le code pour dépiler les variables locales à la fin
    (setq i 0)  ; Réinitialisation du compteur
    (loop while (< i (length bindings)) do
      (setq teardown-code
            (append teardown-code '((POP :R1))))
      (setq i (1+ i)))  ; Incrémenter l'indice
    ;; Combiner tout le code : initialisation, corps, nettoyage
    (append setup-code (compilation (third expr) updated-env) teardown-code)
  )
)
(defun compile-lit (expr env)
  "Compilation d'un littéral-> variable local ou TODO globale, constante"
  (let ((variable-local (assoc expr env)));(x décalage_de_x_dans_la_pile) par exemple
    (format t "~%env : ~A ~%" env)
    ;(format t "~%variable local : ~A~%" variable-local)
    (if variable-local;Si c'est une variable locale
      (append
          `((MOVE :FP :R0));Charge le pointeur de cadre (FP) dans R0.
          `((ADD  ,(cdr variable-local) :R0)); Déduit la position de la variable dans la pile.
          `((LOAD :R0 :R0))                         ;; Charge la valeur de la variable à partir de l'adresse calculée.
      )   
      (if (null expr)
        '((MOVE 0 :R0));Compilation de NIL
        `((MOVE ,expr :R0));Compilation d'une constante
      )
    )
  )
)
(defun compile-while (expr env)
  "Compile une boucle de la forme (loop while <condition> do <instructions>)"
  (let ((etiq-fin (gensym "finwhile"))
	      (etiq-boucle (gensym "while"))) 
    (append 
      `((LABEL ,etiq-boucle))
      (compilation (third expr) env);Compilation de la condition
      '((CMP :R0 0));Si la condition du while est/devient fausse 
      `((JEQ ,etiq-fin));on sort de la boucle
      (compilation (fifth expr) env);Compilation de l'intérieur du while
      `((JMP ,etiq-boucle));on continue de boucler
      `((LABEL ,etiq-fin))
    )
  )
)
(defun compile-op (expr env)
  "Compile une opération de la forme (op expr1 expr2) où op est parmi {+,-,*,/}"
  (append 
		(compilation (second expr) env);Compile expr1 et met le résultat dans R0
    '((PUSH :R0));On empile R0
    (compilation (third expr) env);Compile expr2 et met le résultat dans R0
    '((PUSH :R0))
		'((POP :R1));On récupère le résultat des nos 2 expressions compilées
		'((POP :R0))
		(case (first expr);Cas sur l'opérateur arithmétique
		  ('+ '((ADD :R1 :R0)))
		  ('- '((SUB :R1 :R0)))
		  ('* '((MUL :R1 :R0)))
		  ('/ '((DIV :R1 :R0))))
  )
)

(defun compile-comp (expr env)
  "Compile une comparaison de la forme (op expr1 exp2) où op est parmi {=,>,<,>=,<=}
  Le résultat de la comparaison est 0(->false) ou 1(->true) dans R0"
  (let ((etiq-fin (gensym "finTest")))
    (append 
        (compilation (second expr) env);On compile la première partie de la comparaison
        '((PUSH :R0));On empile R0
        (compilation (third expr) env);On compile la deuxième partie de la comparaison
        '((PUSH :R0))
        '((POP :R1));On récupère le résultat des nos 2 expressions compilées
        '((POP :R0))
        '((CMP :R0 :R1)); On compare les deux parties de la comparaisons
        '((MOVE 1 :R0)) 
        (case (first expr);Cas sur l'opérateur de comparaison
          ('= `((JEQ ,etiq-fin)))
          ('< `((JLT ,etiq-fin)))
          ('> `((JGT ,etiq-fin)))
          ('<= `((JLE ,etiq-fin)))
          ('>= `((JGE ,etiq-fin)))
        )
        ;Cas où la condition s'avère fausse, aucun saut effectué par les J
        '((MOVE 0 :R0))
        `((LABEL ,etiq-fin)))
  )
)
(defun compile-if (expr env)
  "Compile un if statement de la forme (if condition expr-alors expr-sinon)"
  (let ((etiq-else (gensym "else"))
	      (etiq-endif (gensym "endif")))
    (append
    (compilation (second expr) env);Compilation de la condition du if->met le résultat dans :R0
    `((CMP :R0 0))
    `((JEQ ,etiq-else));Condition fausse, JMP au sinon, ou condition vrai, on continue 
    (compilation (third expr) env) ;Compiler le alors
    `((JMP ,etiq-endif));Sauter par dessus le sinon
    `((LABEL ,etiq-else))
    (compilation (fourth expr) env);Compiler le sinon
    `((LABEL ,etiq-endif))
    )
  )
)