(load "./eopl.ss")


(define-datatype Queue Queue?
    (Queue_Default
        (mlist list?)
    )
)

(define Queue_createEmpty
    (lambda ()
        (Queue_Default '())
    )
)

(define Queue_isEmpty
    (lambda (queue)
        (cases Queue queue
           (Queue_Default (mlist)
                (eqv? mlist '())
            )
            (else
                (error 'Queue_isEmpty "It's not a queue.")
            )
        )
    )
)

(define Queue_enque
    (lambda (queue elem)
        (cases Queue queue
            (Queue_Default (mlist)
                (Queue_Default
                    (append mlist 
                        (list elem)
                    )
                )  
            )
            (else
                (error 'Queue_enque "It's not a queue.")
            )
        )
    )
)

(define Queue_deque
    (lambda (queue func)
        (cases Queue queue
            (Queue_Default (mlist) 
                (func
                    (car mlist)
                    (Queue_Default 
                        (cdr mlist)
                    )
                )
            )
            (else
                (error 'Queue_deque "It's not a queue.")
            )
        )
    )
)