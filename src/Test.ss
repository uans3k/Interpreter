(load "./eopl.ss")
(load "./ExpVal.ss")
(load "./Env.ss")
(load "./ListTools.ss")

; (define-datatype UObject UObject?
;     (EmptyObject)
;     (JObject
;         (num number?)
;     )
;     (PObject
;         (num number?)
;     )
; )

; (cases UObject (JObject 1)
;     (JObject (num) (display "JObject"))
;     (PObject (num) (display "PObject"))
;     (else (display "wakaka"))
; )

; ; (display 
; ;     (UObject? (JObject 1)
; ;     )
; ; )

; (define x
;     10
; )

; (define multiPro
;     (lambda ()
;         (set! x 6)
;         (let ((y 4))
;            (+ 2 y)
;         )
;     )
; )

; (display (+ (multiPro) x)
; )


; (define err error)

; (err 'Interpreter_getExpVal "the right of Div must not be 0")


; (define testExpVal
;     (ExpVal_Number 1)
; )

; (define create
;     (lambda (type arg)
;       (type arg)
;     )
; )

; (define changeTestExpVal
;     (lambda (expVal)
;         (cases ExpVal expVal
;             (ExpVal_Number (num)
;                 (set)
;             )
;             (else 1)
;         )
;     )
; )
; (define n 10)
; (eopl:printf "asd : ~s" n)


; (display 
;     (procedure?
;        (lambda ()
;            1
;        )
;     )
; )


; (define testLet
;     (lambda (n)
;         (let rest ((n n))
;             (if (= n 0)
;                 (display "end")
;                 (begin
;                     (display n)
;                     (rest (- n 1))
;                 )
;             )
;         )
;     )
; )

; (define list-index
;     (lambda (pred lst)
;       (cond
;          ((null? lst) #f)
;          ((pred (car lst)) 0)
;          ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
;          (else #f))))

; (display  (list-index
;             (lambda (e)
;                (not (= e 1))
;             )
;             (list 1 1 1 2 1)
;           )
; )

(display
    (ListTools_index
        (list 0 1 1 1 2 2 2 3 3 3)
        0
        (lambda (item)
            (= item 0)
        )
    )
)

(exit)