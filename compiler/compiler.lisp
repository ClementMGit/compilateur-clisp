(defun compilation (expr param-env local-var-env)
  (if (atom expr)
    (compile-lit expr param-env local-var-env)
    (let ((first-symbol (car expr)))
      (cond
      ((eql first-symbol 'let) (compile-let expr param-env local-var-env))
      ((eql first-symbol 'let*) (compile-let-star expr param-env local-var-env))
      ((eql first-symbol 'and) (compile-and (cdr expr) (gensym "finAnd") param-env local-var-env))
      ((eql first-symbol 'or) (compile-or (cdr expr) (gensym "finOr") param-env local-var-env))
      ((eql first-symbol 'if) (compile-if expr param-env local-var-env))
      ((eql first-symbol 'loop) (compile-while expr param-env local-var-env))
      ((eql first-symbol 'setq) (compile-setq expr param-env local-var-env))
      ((eql first-symbol 'cond) (compile-cond (cdr expr) (gensym "fincond") param-env local-var-env))
      ((eql first-symbol 'defun) (compile-defun expr param-env local-var-env))
      ((eql first-symbol 'progn) (compile-progn (cdr expr) param-env local-var-env))
      ((member first-symbol '(< > = <= >= )) (compile-comp expr param-env local-var-env))
      ((member first-symbol '(+ - * /)) (compile-op expr param-env local-var-env))
      (t (compile-fcall expr param-env local-var-env))
      )
    )
  )
)
(defun compile-fichier (file-name output-file-name)
  "Compile un fichier .lisp en fichier .asm"
  (let* ((file (open file-name))
        (expr (read file nil))
        (lisp-code '())
        (asm-code '()))

  (loop while expr do
    (setq lisp-code (append lisp-code (list expr)))
    (setq expr (read file nil))
  )
  (close file)
  (setq asm-code (compile-progn lisp-code '() '()))
  (with-open-file (str output-file-name
                           :direction :output        ;; Ouvre en mode écriture.
                           :if-exists :supersede     ;; Écrase le fichier existant.
                           :if-does-not-exist :create) ;; Crée un fichier s'il n'existe pas.
    (format str "~A" (write-to-string asm-code))
  )
  )
)
(defun compile-or (expr etiq-fin param-env local-var-env)
  "Compile une expression or de la forme (or expr1 expr2)"
    (if (null expr) 
    (append 
      '((MOVE 0 :R0)) 
      `((LABEL ,etiq-fin)))
    (append 
      (compilation (car expr) param-env local-var-env) 
	    '((CMP :R0 1))
	    `((JEQ ,etiq-fin))
	    (compile-or (cdr expr) etiq-fin param-env local-var-env))
  )
)
(defun compile-and (expr etiq-fin param-env local-var-env)
  "Compile une expression and de la forme (and expr1 expr2)"
  (if (null expr) 
    (append 
      '((MOVE 1 :R0)) 
      `((LABEL ,etiq-fin)))
    (append 
      (compilation (car expr) param-env local-var-env) 
	    '((CMP :R0 1))
	    `((JNE ,etiq-fin))
	    (compile-and (cdr expr) etiq-fin param-env local-var-env))
  )
)
(defun compile-let-star (expr param-env local-var-env)
  "Compile une expression let* de la forme (let* ((nomvar1 valeurvar1) ... (nomvarn valeurvarn)) expr)
  Compile séquentiellement les variables contrairement à let"  
)
(defun compile-defun (expr param-env local-var-env)
  "Compile une définition de fonction de la forme (defun nomfonction (paramètres) corps)"
  (let ((positionPile 1)
        (etiq-fin (gensym "findefun")))
    (dolist (param (third expr)) ;; Itère sur chaque paramètre.
    (setq param-env (acons param (- 0 positionPile) param-env));; Met à jour param-env
    (setq positionPile (+ positionPile 1)) ;; Incrémente la position.
    ) 
    (append 
    `((JMP ,etiq-fin));C'est une déclaration de fonction, on ne doit pas exécuter son contenu sans passer par un jsr
    `((LABEL ,(second expr)));Label nomfonction
    (compilation (fourth expr) param-env local-var-env);Compilation du corps de la fonction
    '((RTN))
    `((LABEL ,etiq-fin))
    )
  )
)
(defun compile-fcall (expr param-env local-var-env)
  "Compile un appel de fonction de la forme (nomfonction arg1..argn)"
  (let* ((nom-fonction (car expr))      ;; Le nom de la fonction appelée.
        (arguments (cdr expr))         ;; Les arguments de l'appel de fonction.
        (nb-arguments (length arguments))) ;; Le nombre d'arguments.
    (append
     ;; Compilation des arguments et leur empilement.
    (apply 'append 
      (map 'list
      (lambda (arg)
        (append
         (compilation arg param-env local-var-env); Compile chaque argument.
         '((PUSH :R0)); Empile sa valeur
        )
      )
      (reverse arguments))
    )
    '((MOVE :FP :R1)); Sauvegarde old FP 
    '((MOVE :SP :FP));nouveau FP doit pointer sur nb args
    `((PUSH ,nb-arguments)); Empile le nombre d'arguments.
    ;; Calcul de l'ancien SP.
    `((MOVE :SP :R2))
    `((SUB ,(+ nb-arguments 1) :R2))
    '((PUSH :R2)); Empile old SP.
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
(defun compile-cond (expr etiq-fin param-env local-var-env)
  "Compile un cond de la forme (cond ((test1) (expr1)) ((test2) (expr2)) )"
  (if (null expr)
    `((LABEL ,etiq-fin));Fin du cond 
    (let ((etiq-cond (gensym "cond")))                              
        (append (
          compilation (caar expr) param-env local-var-env);Compilation de la condition             
          '((CMP :R0 0))                                
          `((JEQ ,etiq-cond))                             
          (compilation (cadar expr) param-env local-var-env)              
          `((JMP ,etiq-fin))                               
          `((LABEL ,etiq-cond))                                    
          (compile-cond (cdr expr) etiq-fin param-env local-var-env))
    )
  )
)
(defun compile-progn (expr param-env local-var-env)
  (if (null expr) 
    ()
    (append (compilation (car expr) param-env local-var-env) (compile-progn (cdr expr) param-env local-var-env))
  )
)
(defun compile-setq (expr param-env local-var-env)
  "Compile une expression setq de la forme (setq variable nouvelle-valeur)"
  (let ((variable-local (or (assoc (second expr) param-env) (assoc (second expr) local-var-env))))  ; Recherche de la variable dans l'environnement
    (if variable-local  ; Si la variable est déjà locale
        (append 
         (compilation (third expr) param-env local-var-env); Compiler la valeur à affecter
         `((STORE :R0 (:FP ,(cdr variable-local))))); Déplacer la valeur dans la variable locale
    )
  )
)


(defun compile-let (expr param-env local-var-env)
  "Compile une expression let de la forme (let ((nomvar1 valeurvar1) ... (nomvarn valeurvarn)) expr)"
  (let* ((bindings (second expr)); Les bindings locaux dans le let
         (updated-env local-var-env)
         (setup-code '())
         (teardown-code '()) ; Code pour nettoyer après le let
         (i 0))
    ;; Initialisation des variables locales et mise à jour de l'environnement
    (loop while (< i (length bindings)) do
      (let* ((binding (nth i bindings))     ; Chaque binding
             (var-name (first binding))      ; Nom de la variable
             (var-init (second binding)))    ; Valeur initiale de la variable
        ;On ajoute a l'env une nouvelle association (nom_de_variable decalage_dans_la_pile)
        (setq updated-env (cons (cons var-name (+ (length updated-env) 4)) updated-env))
        ;; Compiler la valeur initiale et empiler
        (setq setup-code
              (append setup-code
                      (compilation var-init param-env local-var-env)
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
    (append setup-code (compilation (third expr) param-env updated-env) teardown-code)
  )
)
(defun compile-lit (expr param-env local-var-env)
  "Compile un littéral : variable locale, paramètre de fonction ou constante."
  (let ((variable (or (assoc expr param-env) (assoc expr local-var-env))))
    (cond
      ;; Si la variable est trouvée dans l'un des environnements
      (variable
       `((LOAD (:FP ,(cdr variable)) :R0)))
      ;; Si l'expression est NIL
      ((null expr)
       '((MOVE 0 :R0)))
      ;; Sinon, on suppose que c'est une constante
      (t
      `((MOVE ,expr :R0)))))
)
(defun compile-while (expr param-env local-var-env)
  "Compile une boucle de la forme (loop while <condition> do <instructions>)"
  (let ((etiq-fin (gensym "finwhile"))
	      (etiq-boucle (gensym "while"))) 
    (append 
      `((LABEL ,etiq-boucle))
      (compilation (third expr) param-env local-var-env);Compilation de la condition
      '((CMP :R0 0));Si la condition du while est/devient fausse 
      `((JEQ ,etiq-fin));on sort de la boucle
      (compilation (fifth expr) param-env local-var-env);Compilation de l'intérieur du while
      `((JMP ,etiq-boucle));on continue de boucler
      `((LABEL ,etiq-fin))
    )
  )
)
(defun compile-op (expr param-env local-var-env)
  "Compile une opération de la forme (op expr1 expr2) où op est parmi {+,-,*,/}"
  (append 
		(compilation (second expr) param-env local-var-env);Compile expr1 et met le résultat dans R0
    '((PUSH :R0));On empile R0
    (compilation (third expr) param-env local-var-env);Compile expr2 et met le résultat dans R0
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

(defun compile-comp (expr param-env local-var-env)
  "Compile une comparaison de la forme (op expr1 exp2) où op est parmi {=,>,<,>=,<=}
  Le résultat de la comparaison est 0(->false) ou 1(->true) dans R0"
  (let ((etiq-fin (gensym "finTest")))
    (append 
        (compilation (second expr) param-env local-var-env);On compile la première partie de la comparaison
        '((PUSH :R0));On empile R0
        (compilation (third expr) param-env local-var-env);On compile la deuxième partie de la comparaison
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
(defun compile-if (expr param-env local-var-env)
  "Compile un if statement de la forme (if condition expr-alors expr-sinon)"
  (let ((etiq-else (gensym "else"))
	      (etiq-endif (gensym "endif")))
    (append
    (compilation (second expr) param-env local-var-env);Compilation de la condition du if->met le résultat dans :R0
    `((CMP :R0 0))
    `((JEQ ,etiq-else));Condition fausse, JMP au sinon, ou condition vrai, on continue 
    (compilation (third expr) param-env local-var-env) ;Compiler le alors
    `((JMP ,etiq-endif));Sauter par dessus le sinon
    `((LABEL ,etiq-else))
    (compilation (fourth expr) param-env local-var-env);Compiler le sinon
    `((LABEL ,etiq-endif))
    )
  )
)