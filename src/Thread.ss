(load "./Queue.ss")

(define Thread_readyQueue 'undefine)
(define Thread_finalAnswer 'undefine)
(define Thread_timeSlice 'undefine)
(define Thread_timeRemain 'undefine)


(define Thread_init!
    (lambda (timeSlice)
        (begin
            (set! Thread_readyQueue (Queue_createEmpty))
            (set! Thread_finalAnswer "Error!")
            (set! Thread_timeSlice timeSlice)
            (set! Thread_timeRemain Thread_timeSlice)
        )
    )
)

(define Thread_setFinalAnswer!
    (lambda (expVal)
        (set! Thread_finalAnswer expVal)
    )
)

; (()->ExpVal) -> undefine 
(define Thread_enque!
    (lambda (th)
        (set! Thread_readyQueue 
            (Queue_enque Thread_readyQueue th)
        )
    )
)

;()->boolean
(define Thread_isTimeExpire
    (lambda ()
        (zero? Thread_timeRemain)
    )
)

(define Thread_timeDecrement!
    (lambda ()
        ; (eopl:printf "current remain time : ~s" Thread_timeRemain)
        ; (display #\newline)
        (set! 
            Thread_timeRemain 
            (- Thread_timeRemain 1)
        )
    )
)

(define Thread_runNext!
    (lambda ()
        (if (Queue_isEmpty Thread_readyQueue)
            Thread_finalAnswer
            (Queue_deque Thread_readyQueue
                (lambda (th queueRemain)
                    (begin
                        (set! Thread_readyQueue queueRemain)
                        (set! Thread_timeRemain Thread_timeSlice)
                        (th)
                    )              
                )
            )
        )
    )
)

(define-datatype Semaphore Semaphore?
    (Semaphore_Vector
        (vect vector?)
    )
)

(define Semaphore_create
    (lambda (count)
        (Semaphore_Vector (vector (Queue_createEmpty) count))
    )
)

(define Semaphore_p!
    (lambda (semaphore th)
        (cases Semaphore semaphore
            (Semaphore_Vector(vect)
              
                (let
                    (
                        (queue (vector-ref vect 0))
                        (count (vector-ref vect 1))
                    )
                    (begin
                        (vector-set! vect 1 
                            (- count 1)
                        )
                        (if (<= count 0)
                            (begin
                                (vector-set! vect 0
                                    (Queue_enque queue th) 
                                )
                                (Thread_runNext!)
                            )
                            (th)
                        )
                       
                    )
                )
            )
            (else
                (error 'Semaphore_p "can't P a non-Semaphore : ~s" semaphore)
            )
        )
    )
)

(define Semaphore_v!
    (lambda (semaphore)
        (cases Semaphore semaphore
            (Semaphore_Vector (vect)
                (let
                    (
                        (queue (vector-ref vect 0))
                        (count (vector-ref vect 1))
                    )
                    (begin
                        (vector-set! vect 0 
                            (+ count 1)
                        ) 
                        (if (< count 0)
                            (Queue_deque queue
                                (lambda (th queueRemain)
                                    (begin
                                        (Thread_enque! th)
                                        (vector-set! vect 0 queueRemain)
                                    )
                                )
                            )      
                        )
                    )
                )
            )
            (else
                (error 'Semaphore_v "can't V a non-Semaphore : ~s" semaphore)
            )
        )
    )
)