(defun compilation (expr)
  (cond
    ((atom expr) (compile-lit expr))
    ((eql (car expr) 'if) (compile-if expr))
    ((member (car expr) '(< > = <= >= )) (compile-comp expr))
    ((member (car expr) '(+ - * /)) (compile-op expr))
    ;((is-cas exp 'and) (compilation-and arg (gensym "finAnd")))
    ;((is-cas exp 'or) (compilation-or arg (gensym "finOr")))
  )
)
(defun compile-op (expr)
  "Compile une opération de la forme (op expr1 expr2) où op est parmi {+,-,*,/}"
  (append 
		(compilation (second expr));Compile expr1 et met le résultat dans R0
    '((PUSH :R0));On empile R0
    (compilation (third expr));Compile expr2 et met le résultat dans R0
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

(defun compile-comp (expr)
  "Compile une comparaison de la forme (op expr1 exp2) où op est parmi {=,>,<,>=,<=}"
  (let ((etiq-fin (gensym "finTest")))
    (append 
        (compilation (second expr));On compile la première partie de la comparaison
        '((PUSH :R0));On empile R0
        (compilation (third expr));On compile la deuxième partie de la comparaison
        '((PUSH :R0))
        '((POP :R1));On récupère le résultat des nos 2 expressions compilées
        '((POP :R0))
        '((CMP :R0 :R1)); On compare les deux parties de la comparaisons
        '((MOVE T :R0)) 
        (case (first expr);Cas sur l'opérateur de comparaison
          ('= `((JEQ ,etiq-fin)))
          ('< `((JLT ,etiq-fin)))
          ('> `((JGT ,etiq-fin)))
          ('<= `((JLE ,etiq-fin)))
          ('>= `((JGE ,etiq-fin)))
        )
        ;Cas où la condition s'avère fausse, aucun saut effectué par les J
        '((MOVE NIL :R0))
        `((LABEL ,etiq-fin)))
  )
)
(defun compile-if (expr)
  "Compile un if statement de la forme (if condition expr-alors expr-sinon)"
  (let ((etiq-else (gensym "else"))
	      (etiq-endif (gensym "endif"))
        (condition (second expr))
        (alors (third expr))
        (sinon (fourth expr))
        )
    (append
    (compilation condition);Compilation de la condition du if->met le résultat dans :R0
    `((CMP :R0 0))
    `((JEQ ,etiq-else));Condition fausse, JMP au sinon, ou condition vrai, on continue 
    (compilation alors) ;Compiler le alors
    `((JMP ,etiq-endif));Sauter par dessus le sinon
    `((LABEL ,etiq-else))
    (compilation sinon);Compiler le sinon
    `((LABEL ,etiq-endif))
    )

  )
)
(defun compile-lit (expr)
  (if (null expr) 
    `((MOVE 0 ,:R0));On remplace un nil par un 0 en assembleur
    `((MOVE ,expr :R0))
  )
)