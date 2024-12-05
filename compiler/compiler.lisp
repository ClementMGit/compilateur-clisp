(defun compilation (expr &optional (env ()) (fenv ())  (nomf ()) )
  (let ((arg (if (atom expr) () (cdr exp))))
    (cond
      ((atom expr) (compile-lit expr env fenv nomf))
      ((eql (car expr) 'if) (compile-if arg env fenv nomf))

     ;((member (car exp) '(+ - * /)) (compilation-op exp env fenv nomf))
     ;((member (car exp) '(< > = <= >= )) (compilation-comp exp env fenv nomf))
     ;((is-cas exp 'and) (compilation-and arg (gensym "finAnd") env fenv nomf))
     ;((is-cas exp 'or) (compilation-or arg (gensym "finOr") env fenv nomf))
    )
  )
)
(defun compile-if (expr, env, fenv, nomf)
  "Compile un If statement de la forme Si cond alors expr1 sinon expr2"
  (let ((etiq-else (gensym "else"))
	      (etiq-endif (gensym "endif")))
    ;Compilation de la condition du if->met le résultat dans :R0
    (compilation (first expr))

  )
)
(defun compile-lit (exp env fenv nomf)
  `(MOVE ,exp :R0)
)