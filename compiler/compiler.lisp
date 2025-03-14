(defun compilation (expr param-env local-var-env)
  (if (atom expr) 
    (compile-lit expr param-env local-var-env)
    (let ((first-symbol (car expr))
          (cdr-expr (cdr expr)))
      (format t "compilation de ~A~%" expr)
      (cond
      ((and (listp expr) (not (symbolp first-symbol)))(compile-progn expr param-env local-var-env))
      ((and (listp expr) (eql first-symbol 'quote)) (compile-lit (second expr) param-env local-var-env))
      ((and (listp expr) (eq (intern (symbol-name first-symbol)) 'backquote)) (compile-backquote expr param-env local-var-env))
      ((eql first-symbol 'let) (compile-let cdr-expr param-env local-var-env))
      ((eql first-symbol 'let*) (compile-let-star cdr-expr param-env local-var-env))
      ((eql first-symbol 'and) (compile-and cdr-expr (gensym "finAnd") param-env local-var-env))
      ((eql first-symbol 'or) (compile-or cdr-expr (gensym "finOr") param-env local-var-env))
      ((eql first-symbol 'if) (compile-if expr param-env local-var-env))
      ((eql first-symbol 'loop) (compile-while expr param-env local-var-env))
      ((eql first-symbol 'setq) (compile-setq expr param-env local-var-env))
      ((eql first-symbol 'cond) (compile-cond cdr-expr (gensym "fincond") param-env local-var-env))
      ((eql first-symbol 'defun) (compile-defun expr param-env local-var-env))
      ((eql first-symbol 'progn) (compile-progn cdr-expr param-env local-var-env))
      ((member first-symbol '(< > = <= >= )) (compile-comp expr param-env local-var-env))
      ((member first-symbol '(+ - * /)) (compile-op expr param-env local-var-env))
      (t (compile-fcall expr param-env local-var-env))
      )
    )
  )
)
(defun compile-backquote (expr param-env local-var-env)
  ;"Compile une expression backquotée"
  (let ((expr (macroexpand expr)))
    (compilation expr param-env local-var-env)
  ) 
)

(defun compile-fichier (file-name output-file-name)
  ;"Compile un fichier .lisp en fichier .asm"
  (let* ((file (open file-name))
        (expr (read file nil))
        (lisp-code '())
        (asm-code '()))
  ;On itère sur chaque expression lisp du fichier
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
  ;"Compile une expression or de la forme (or expr1 expr2)"
  (if (null expr) 
    (append 
      '((MOVE (LIT 0) :R0)) 
      `((LABEL ,etiq-fin)))
    (append 
      (compilation (car expr) param-env local-var-env) 
	    '((CMP :R0 (LIT 1)))
	    `((JEQ ,etiq-fin))
	    (compile-or (cdr expr) etiq-fin param-env local-var-env))
  )
)
(defun compile-and (expr etiq-fin param-env local-var-env)
  ;"Compile une expression and de la forme (and expr1 expr2)"
  (if (null expr) 
    (append 
      '((MOVE (LIT 1) :R0)) 
      `((LABEL ,etiq-fin)))
    (append 
      (compilation (car expr) param-env local-var-env) 
	    '((CMP :R0 (LIT 1)));;Si (cmp obj 1)
	    `((JNE ,etiq-fin))
	    (compile-and (cdr expr) etiq-fin param-env local-var-env))
  )
)
(defun compile-defun (expr param-env local-var-env)
  ;"Compile une définition de fonction de la forme (defun nomfonction (paramètres) corps)"
  (let ((positionPile 1)
        (etiq-fin (gensym "findefun"))
        (liste-param (third expr)))
    (loop while liste-param do 
      (setq param-env (acons (car liste-param) (- 0 positionPile) param-env));; Met à jour param-env
      (setq positionPile (+ positionPile 1)) ;; Incrémente la position
      (setq liste-param (cdr liste-param))
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
  ;"Compile un appel de fonction de la forme (nomfonction arg1..argn)"
  (let* ((nom-fonction (car expr))
        (arguments (reverse (cdr expr)))
        (nb-arguments (length arguments))
        (compiled-args '())) ;
    
    (loop while arguments do 
      (setq compiled-args (append compiled-args
        (compilation (car arguments) param-env local-var-env)
        '((PUSH :R0)))
      )
      (setq arguments (cdr arguments))
    )
    (append
    compiled-args
    '((MOVE :FP :R1)); Sauvegarde old FP 
    '((MOVE :SP :FP));nouveau FP doit pointer sur nb args
    `((PUSH (LIT ,nb-arguments))); Empile le nombre d'arguments.
    ;; Calcul de l'ancien SP.
    `((MOVE :SP :R2))
    `((SUB (LIT ,(+ nb-arguments 1)) :R2))
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
  ;"Compile un cond de la forme (cond ((test1) (expr1)) ((test2) (expr2)) )"
  (if (null expr)
    `((LABEL ,etiq-fin));Fin du cond 
    (let ((etiq-cond (gensym "cond")))                              
        (append (
          compilation (caar expr) param-env local-var-env);Compilation de la condition             
          '((CMP :R0 (LIT 0)))                                
          `((JEQ ,etiq-cond))                             
          (compilation (cadar expr) param-env local-var-env)              
          `((JMP ,etiq-fin))                               
          `((LABEL ,etiq-cond))                                    
          (compile-cond (cdr expr) etiq-fin param-env local-var-env))
    )
  )
)
(defun compile-progn (expr param-env local-var-env)
  ;; Compile une expression progn de la forme (progn expr1 ... exprn)
  (if (null expr) 
      ()
    (append (compilation (car expr) param-env local-var-env) (compile-progn (cdr expr) param-env local-var-env))
  )
)
(defun compile-setq (expr param-env local-var-env)
  ;"Compile une expression setq de la forme (setq variable nouvelle-valeur)"
  (let ((variable-local (if (assoc (second expr) param-env)  (assoc (second expr) param-env) (if (assoc (second expr) local-var-env) (assoc (second expr) local-var-env)))))  ; Recherche de la variable dans l'environnement
    (if variable-local  ; Si la variable est déjà locale
        (append 
         (compilation (third expr) param-env local-var-env); Compiler la valeur à affecter
         `((STORE :R0 (:FP ,(cdr variable-local))))); Déplacer la valeur dans la variable locale
    )
  )
)
(defun compile-let-star (expr param-env local-var-env)
  ;"Compile une expression let* de la forme (let* ((nomvar1 valeurvar1) ... (nomvarn valeurvarn)) expr).
  ;Transforme let* en une série de let imbriqués et le compile."
  (let* ((bindings (reverse (first expr)))  ; Les paires (var valeur)
         (body (rest expr)))    ; Initialise l'expression finale
    ;; Parcourir les bindings en partant de la fin pour imbriquer les let
    (loop while bindings do
      (let ((binding (car bindings)))
        (setq body `(let ((,(first binding) ,(second binding))) ,body))
      )
      (setq bindings (cdr bindings))
    )
    ;; Compiler l'expression transformée
    (compilation body param-env local-var-env))
)
(defun compile-let (expr param-env local-var-env)
  ;"Compile une expression let de la forme (let ((nomvar1 valeurvar1) ... (nomvarn valeurvarn)) expr)"
  (let* ((bindings (first expr)); Les bindings locaux dans le let
        (updated-env local-var-env)
        (nb-pop (length bindings))
        (setup-code '())
        (body-code '())
        (teardown-code '())
        (body (rest expr))
        (i 0))
    ;; Initialisation des variables locales et mise à jour de l'environnement
    (loop while bindings do
      (let* ((binding (car bindings))     ; Chaque binding
             (var-name (first binding))      ; Nom de la variable
             (var-init (second binding)))    ; Valeur initiale de la variable
        ;On ajoute a l'env une nouvelle association (nom_de_variable decalage_dans_la_pile)
        (setq updated-env (cons (cons var-name (+ (length updated-env) 4)) updated-env))
        ;; Compiler la valeur initiale et empiler
        (setq setup-code
              (append setup-code
                      (compilation var-init param-env local-var-env)
                      '((PUSH :R0))))
        (setq bindings (cdr bindings))
      )
    )  
    ;; Générer le code pour dépiler les variables locales à la fin
    (loop while (< i nb-pop) do
      (setq teardown-code
            (append teardown-code '((POP :R1))))
      (setq i (+ i 1)))  ; Incrémenter l'indice

    ; Combiner tout le code : initialisation, corps, nettoyage
    (append setup-code (compile-progn body param-env updated-env) teardown-code)
  )
)
(defun compile-lit (expr param-env local-var-env)
  ;"Compile un littéral : variable locale, paramètre de fonction ou constante."
  (let ((variable (if (assoc expr param-env) (assoc expr param-env) (if (assoc expr local-var-env) (assoc expr local-var-env)))))
    (cond
      ;; Si la variable est trouvée dans l'un des environnements     
      (variable
       `((LOAD (:FP ,(cdr variable)) :R0)))     
      ;; Sinon, on suppose que c'est une constante
      (t
      `((MOVE (LIT ,expr) :R0)))))
)
(defun compile-while (expr param-env local-var-env)
  ;"Compile une boucle de la forme (loop while <condition> do <instructions>)"
  (let* ((etiq-fin (gensym "finwhile"))     ; Étiquette pour la fin de la boucle
         (etiq-boucle (gensym "while"))     ; Étiquette pour le début de la boucle
         (body-code '())                    ; Code pour le corps de la boucle
         (body (rest (rest (rest expr)))))  ; Corps de la boucle (après le "while")
    (append 
     `((LABEL ,etiq-boucle))   ; Étiquette du début de la boucle
     (compilation (third expr) param-env local-var-env) ; Compilation de la condition
     '((CMP :R0 (LIT 0)))     ; Comparaison de la condition avec 0
     `((JEQ ,etiq-fin))       ; Sortie si la condition est fausse
     (compile-progn body param-env local-var-env); Corps de la boucle
     `((JMP ,etiq-boucle))    ; Retour au début de la boucle
     `((LABEL ,etiq-fin))     ; Étiquette de fin de la boucle
     )
  )
)
(defun compile-op (expr param-env local-var-env)
  ;"Compile une opération de la forme (op expr1 expr2) où op est parmi {+,-,*,/}"
  (let ((first-expr (first expr)))
    (append 
      (compilation (second expr) param-env local-var-env);Compile expr1 et met le résultat dans R0
      '((PUSH :R0));On empile R0
      (compilation (third expr) param-env local-var-env);Compile expr2 et met le résultat dans R0
      '((PUSH :R0))
      '((POP :R1));On récupère le résultat des nos 2 expressions compilées
      '((POP :R0))
      ;Cas sur l'opérateur arithmétique
      (cond ((eql first-expr '+) '((ADD :R1 :R0)))
        ((eql first-expr '-) '((SUB :R1 :R0)))
        ((eql first-expr '*) '((MUL :R1 :R0)))
        ((eql first-expr '/) '((DIV :R1 :R0)))
        )
    )
  )
)
(defun compile-comp (expr param-env local-var-env)
  ;"Compile une comparaison de la forme (op expr1 exp2) où op est parmi {=,>,<,>=,<=}
  ;Le résultat de la comparaison est 0(->false) ou 1(->true) dans R0"
  (let ((etiq-fin (gensym "finTest"))
        (first-expr (first expr)))
    (append 
        (compilation (second expr) param-env local-var-env);On compile la première partie de la comparaison
        '((PUSH :R0));On empile R0
        (compilation (third expr) param-env local-var-env);On compile la deuxième partie de la comparaison
        '((PUSH :R0))
        '((POP :R1));On récupère le résultat des nos 2 expressions compilées
        '((POP :R0))
        '((CMP :R0 :R1)); On compare les deux parties de la comparaisons
        '((MOVE (LIT 1) :R0)) 
        ;Cas sur l'opérateur de comparaison
        (cond ((eql first-expr '=) `((JEQ ,etiq-fin)))
          ((eql first-expr '<) `((JLT ,etiq-fin)))
          ((eql first-expr '>) `((JGT ,etiq-fin)))
          ((eql first-expr '<=) `((JLE ,etiq-fin)))
          ((eql first-expr '>=) `((JGE ,etiq-fin)))
        )
        ;Cas où la condition s'avère fausse, aucun saut effectué par les J
        '((MOVE (LIT 0) :R0))
        `((LABEL ,etiq-fin))
    )
  )
)
(defun compile-if (expr param-env local-var-env)
  ;"Compile un if statement de la forme (if condition expr-alors expr-sinon)"
  (let ((etiq-else (gensym "else"))
	      (etiq-endif (gensym "endif")))
    (append
      (compilation (second expr) param-env local-var-env);Compilation de la condition du if->met le résultat dans :R0
      `((CMP :R0 (LIT 0)))
      `((JEQ ,etiq-else));Condition fausse, JMP au sinon, ou condition vrai, on continue 
      (compilation (third expr) param-env local-var-env) ;Compiler le alors
      `((JMP ,etiq-endif));Sauter par dessus le sinon
      `((LABEL ,etiq-else))
      (compilation (fourth expr) param-env local-var-env);Compiler le sinon
      `((LABEL ,etiq-endif))
    )
  )
)