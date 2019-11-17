(load "./eopl.ss")
(load "./Env.ss")
(load "./Expression.ss")

;First Class Value:
;Number
;Bool
;List
;EmptyList
;Proc
;Ref
(define-datatype ExpVal ExpVal?
    (ExpVal_Number
        (num number?)
    )
    (ExpVal_Bool
        (bool boolean?)
    )
    (ExpVal_LinkList
        (left ExpVal?)
        (right ExpVal?)
    )
    (ExpVal_EmptyListVal)

    (ExpVal_Lambda
        (proc Proc?)
    )
    (ExpVal_Ref
        (ref number?)
    )

    (ExpVal_Semaphore
        (semaphore Semaphore?)
    )
)

(define-datatype Proc Proc?
    (Proc_Vector
        (vect vector?)
    )
)

(define Proc_create
    (lambda (vars body saveEnv)
        (Proc_Vector
            (vector vars body saveEnv)
        )
    )
)

(define Proc_setEnv!
    (lambda (proc env)
        (cases Proc proc
            (Proc_Vector (vect)
                (vector-set! vect 2 env)
            )
            (else 
                (error 'Proc_setEnv "can not set Env for a non-proc")
            )
        )
    )
)


(define Proc_getVars
    (lambda (proc)
        (cases Proc proc
            (Proc_Vector (vect)
               (vector-ref vect 0)
            )
            (else 
              (error 'Proc_setEnv "can not set Env for a non-proc")
            )
        )   
    )
)

(define Proc_getBody
    (lambda (proc)
        (cases Proc proc
            (Proc_Vector (vect)
               (vector-ref vect 1)
            )
            (else 
              (error 'Proc_setEnv "can not set Env for a non-proc")
            )
        )   
    )
)

(define Proc_getEnv
    (lambda (proc)
        (cases Proc proc
            (Proc_Vector (vect)
               (vector-ref vect 2)
            )
            (else 
               (error 'Proc_setEnv "can not set Env for a non-proc")
            )
        )   
    )
)


; (define-datatype Proc Proc?
;     (Proc_Default
;         (vars symbol?)
;         (body Expression?)
;         (saveEnv Env?)
;     )
; )

(define ExpVal_buildLinkList
    (lambda (expVals)
        (ExpVal_LinkList
            (car expVals)
            (ExpVal_buildLinkList 
                (cdr expvals)
            )
        )
    )
)

; ExpVal->SchemeVal
(define ExpVal_getSchemeVal
    (lambda (expVal)
        (cases ExpVal expVal
            (ExpVal_Number (num)
                num
            )
            (ExpVal_Bool (bool)
                bool
            )
            (ExpVal_LinkList (left right)
                (cons (ExpVal_getSchemeVal left) (ExpVal_getSchemeVal right))
            )
            (ExpVal_EmptyListVal ()
                '()
            )

            (ExpVal_Lambda (proc)
                "a lambda"
            )
            
            (ExpVal_Ref (ref)
                ref
            )

            (ExpVal_Semaphore (semaphore)
                "a semaphore"
            )

            (else
                (error 'ExpVal_getSchemeVal "ExpVal type error")
            )
        )

    )
)

;ExpVal->ExpVal
(define ExpVal_car
    (lambda (expVal)
        (cases ExpVal expVal
            (ExpVal_LinkList (left right)
                left
            )
            (else
                (error 'ExpVal_car "arg must be LinkList")
            )
        )
    )
)

;ExpVal->ExpVal
(define ExpVal_cdr
    (lambda (expVal)
        (cases ExpVal expVal
            (ExpVal_LinkList (left right)
                right
            )
            (else
                (error 'ExpVal_cdr "arg must be LinkList")
            )
        )
    )
)