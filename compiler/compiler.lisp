(defun compilation (exp &optional (env ()) (fenv ())  (nomf ()) )
    (cond
     ((atom exp) (compile-lit exp env fenv nomf))
     ;((member (car exp) '(+ - * /)) (compilation-op exp env fenv nomf))
     ;((member (car exp) '(< > = <= >= )) (compilation-comp exp env fenv nomf))
     ;((is-cas exp 'and) (compilation-and arg (gensym "finAnd") env fenv nomf))
     ;((is-cas exp 'or) (compilation-or arg (gensym "finOr") env fenv nomf))
     ;((is-cas exp 'if) (compilation-if arg env fenv nomf))
    )

)

(defun compile-lit (exp env fenv nomf)
  `(MOVE ,exp :R0)
)