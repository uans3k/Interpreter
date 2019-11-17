(define Sto
    'uninit
)


(define Sto_init!
    (lambda ()
        (set! Sto '())
    )
)

;ExpVal -> Number 
(define Sto_newRef!
    (lambda (expVal)
        (let (
                (len (length Sto))
             )
             (begin
                (set! Sto
                    (append Sto (list expVal))
                )
                len
             )
        )
    )
)

;Number -> ExpVal
(define Sto_deRef
    (lambda (ref)
        (list-ref Sto ref)
    )
)

;Number * ExpVal -> Sto
(define Sto_setRef!
    (lambda (ref expVal)
       (letrec (
              (getNewSto
                (lambda (sto inRef)
                  (cond 
                     ((eqv? sto '())
                        (error 'Sto_setRefInner! "invalid references")
                    )
                    ((zero? inRef)
                        (cons expVal (cdr sto))
                    )
                    (else
                        (cons (car sto)
                            (getNewSto
                                (cdr sto)
                                (- inRef 1)
                            )
                        )
                    )
                  )
                )
              )
            )
            (set! Sto
                (getNewSto Sto ref)
            )
       )
    )
)
