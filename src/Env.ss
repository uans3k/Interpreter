(load "./eopl.ss")

(define-datatype Env Env?
    (Env_Default
        (envList list?)
    )
)

(define Env_createDefault
    (lambda ()
        (Env_Default '()
        )
    )
)

;Env * var * ExpVal -> Env
(define Env_put
    (lambda (env var val)
        (cases Env env
            (Env_Default (envList)
                (Env_Default (cons (list var val) envList)
                )
            )
            (else (error 'Env_put "Env type error"))
        )
    )
)

(define Env_putPair
    (lambda (env pair)
        (cases Env env
            (Env_Default (envList)
                (Env_Default (cons pair envList)
                )
            )
            (else (error 'Env_put "Env type error"))
        )
    )
)

(define Env_putPairList
     (lambda (env pairs)
        (if (eqv? pairs '())
            env
            (Env_putPairList
                (Env_putPair env (car pairs))
                (cdr pairs)
            )
        ) 
    )
)


(define Env_putList
    (lambda (env vars vals)
        (if (eqv? vars '())
            env
            (let (
                  (var (car vars))
                  (val (car vals))
                  (varsTail (cdr vars))
                  (valsTail (cdr vals))
                 )
                 (Env_putList
                    (Env_put env var val)
                    varsTail
                    valsTail
                 )
            )
        ) 
    )
)



;Env * var  -> ExpVal
(define Env_get
    (lambda (env var)
        (cases Env env
            (Env_Default (envList)
                (letrec 
                    (
                        (getValue 
                            (lambda (pairList)
                                (if (eqv?  pairList '())
                                     (error 'Env_get "No binding for ~s" var)
                                     (let  (
                                             (head (car pairList))
                                             (tail (cdr pairList))
                                           )
                                           (if (eqv? var (car head))
                                               (cadr head)
                                               (getValue tail)
                                           )
                                     )          
                                   
                                )
                            )
                        )
                    )
                    (getValue envList)
                )
            )
            (else (error 'Env_get "Env type error"))
        )
    )    
)
; (display 
;     (Env_get
;         (Env_put
;             (Env_createDefault)
;             'a
;             1   
;         )
;         'a
;     )
; )
; (Env_put
;     (Env_createDefault)
;     'b
;     1   
; )

