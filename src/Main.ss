(load "./eopl.ss")
; (load "./Paser.ss")
(load "./PaserThreadException.ss")
(load "./Interpreter.ss")
(load "./InterpreterByCont.ss")
(load "./ProgramTest.ss")
(load "./Thread.ss")
(load "./CPSLambda.ss")

; (define pragromString
;     "
;     let even = (x)=>
;                (if =(0,x)
;                 then printn(22222222222222222)
;                 else begin
;                         printn(2);
;                         [odd](-(x,1))
;                      end
;                )
;         #哇哈哈
;         odd = (x)=>
;               (if =(0,x)
;                then printn(1111111111111111)
;                else begin
;                         printn(1);
;                         [even](-(x,1))
;                     end
;               )
;     in  [even](10)
;     "
; )




; (display 
;    (InterpreterByCont_getProgramExpVal (scan&parse Test_S))
; )


; (display 
    ; (InterpreterByCont_runProgram
        ; (scan&parse Test_Thread)
    ; )
; )
(display
    (CPSLambda_program2CPSLambdaProgram
        (scan&parse Test_Var)
    )
)



(exit)


