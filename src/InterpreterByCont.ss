(load "./eopl.ss")
(load "./Expression.ss")
(load "./Env.ss")
(load "./ExpVal.ss")
(load "./Sto.ss")
(load "./Thread.ss")

;Expression * Env * Cont -> ExpVal
(define InterpreterByCont_getExpVal
    (lambda (expression env cont tryCont)
    ;   (lambda ()
        (cases Expression expression

            (Expression_Const (num)
                (Cont_continue cont tryCont
                    (ExpVal_Number num)
                )
            )

            (Expression_Default ()
                (Cont_continue cont tryCont
                    (ExpVal_Bool #t)
                    
                )
            )

            (Expression_True  ()
                (Cont_continue cont tryCont
                    (ExpVal_Bool #t)
                )
            )

             (Expression_False ()
                (Cont_continue cont tryCont
                    (ExpVal_Bool #f)
                )
            )

            (Expression_Diff (left right)
               (InterpreterByCont_getExpVal
                 left
                 env
                 (Cont_DiffRight right env cont)
                 tryCont
               )
            )
            
            (Expression_Add (left right)
               (InterpreterByCont_getExpVal
                 left
                 env
                 (Cont_AddRight right env cont)
                 tryCont
               )
            )

            (Expression_Mult (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_MultRight right env cont)
                  tryCont
               )
            )
            
            (Expression_Div (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_DivRight right env cont)
                  tryCont
               )
            )
            

            (Expression_IsZero (expr)
               (InterpreterByCont_getExpVal
                  expr
                  env
                  (Cont_IsZero cont)
                  tryCont
               )
            )
            
            (Expression_Greater (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_GreaterRight right env cont)
                  tryCont
               )
            )

            (Expression_GreaterEqual (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_GreaterEqualRight right env cont)
                  tryCont
               )
            )

             
            (Expression_Equal (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_EqualRight right env cont)
                  tryCont
               )
            )

             
            (Expression_Less (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_LessRight right env cont)
                  tryCont
               )
            )
              
            (Expression_LessEqual (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_LessEqualRight right env cont)
                  tryCont
               )
            )
            

            (Expression_Cons (left right)
               (InterpreterByCont_getExpVal
                  left
                  env
                  (Cont_ConsRight right env cont)
                  tryCont
               )
            )

            (Expression_Car (expr)
                (InterpreterByCont_getExpVal 
                    expr 
                    env
                    (Cont_Car cont)
                    tryCont
                )
            )

            (Expression_Cdr (expr)
                (InterpreterByCont_getExpVal 
                    expr 
                    env
                    (Cont_Cdr cont)
                    tryCont
                )
            )

            (Expression_EmptyList ()
                (Cont_continue
                    (ExpVal_EmptyListVal)
                    cont
                    tryCont
                )
            )
            
            ;List<Expression>->ExpVal_List<ExpVal>
            (Expression_List (exprs)
                (InterpreterByCont_getExpVal
                    (car exprs)
                    env
                    (Cont_List
                        '()
                        (cdr exprs)
                        env
                        cont
                    )
                    tryCont
                ) 
            )

            (Expression_Cond (conds acts)
                (InterpreterByCont_getExpVal 
                    (car conds)
                    env
                    (Cont_Cond
                        (cdr conds)
                        acts
                        env
                        cont
                    )
                    tryCont
                )
            )



            (Expression_IfElse (predExp trueExp falseExp)
                (InterpreterByCont_getExpVal
                    predExp
                    env
                    (Cont_IfElse trueExp falseExp env cont)
                    tryCont
                )
            )
            
            (Expression_Print (expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_Print cont)
                    tryCont
                )
            )
            
            (Expression_Printn (expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_Printn cont)
                    tryCont
                )
            )
            

            (Expression_Var (var)
                (Cont_continue cont tryCont
                    (Env_get env var)
                )
                
            )

            (Expression_Let (vars exprs body)
               (InterpreterByCont_getExpVal
                   (car exprs)
                   env
                   (Cont_Let
                      vars
                      '()
                      (cdr exprs)
                      body
                      env
                      cont
                   )
                   tryCont
               )
            )

            (Expression_Lambda (vars body)
               (Cont_continue cont tryCont
                  (ExpVal_Lambda
                    (Proc_create vars body env)
                  )
               ) 
            )

            (Expression_ProcCall (expr exprs)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_ProcCallBindValInit
                        exprs
                        env
                        cont
                    )
                    tryCont
                )
            )
            
            (Expression_NewRef (expr)
                (InterpreterByCont_getExpVal 
                    expr 
                    env
                    (Cont_NewRef cont)
                    tryCont
                )
            )
            
            (Expression_DeRef (expr)
                (InterpreterByCont_getExpVal 
                    expr 
                    env
                    (Cont_DeRef cont)
                    tryCont
                )
            )
            
            (Expression_SetRef (left right)
                (InterpreterByCont_getExpVal 
                    left 
                    env
                    (Cont_SetRefRight right env cont)
                    tryCont
                )
            )

            (Expression_Begin (exprs)
                (InterpreterByCont_getExpVal
                    (car exprs)
                    env
                    (Cont_Begin
                        (cdr exprs)
                        env
                        cont
                    )
                    tryCont
                )
            )
            
            (Expression_TryCatch (expr num handler)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_TryCatch cont)
                    (TryCont_Default num handler env cont tryCont)
                )
            )
            
            
            (Expression_Raise (num)
                (TryCont_exceptionHandle tryCont num)
            )

            (Expression_Thread (expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_Thread cont)
                    tryCont
                )
            )

            (Expression_Semaphore (expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_Semaphore cont)
                    tryCont
                )
            )

            (Expression_P(expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_P cont)
                    tryCont
                )
            )

            (Expression_V (expr)
                (InterpreterByCont_getExpVal
                    expr
                    env
                    (Cont_V cont)
                    tryCont
                )
            )




            (else 
                (error 'InterpreterByCont_getExpVal "Expression type error")
            )
        )
    ;   )
    )
)



(define InterpreterByCont_getProcExpVal
    (lambda (expValProc expVals cont tryCont)
      (cases ExpVal expValProc
         (ExpVal_Lambda (proc)
             (let 
                 (
                    (body (Proc_getBody proc))
                    (env (Proc_getEnv proc))
                    (vars (Proc_getVars proc))
                 )
                 (if (= (length vars) (length expVals))
                     (InterpreterByCont_getExpVal 
                        body
                        (Env_putList
                           env
                           vars
                           expVals
                        )
                        cont
                        tryCont
                     )
                     (error 'InterpreterByCont_getExpVal "possible incorrect argument count in call")
                 )
             )
         )
         (else
           (error 'InterpreterByCont_getExpVal "can not call a non-proc")
         )
      )
    )
)



;Env * list<symbol> * list<Expression> -> Env
(define InterpreterByCont_letBind
    (lambda (env vars expVals)
        (begin 
            (InterpreterByCont_procEnvBuild! env vars expVals)
            (Env_putList env vars expVals)        
        )   
    )
)

(define InterpreterByCont_procEnvBuild!
    (lambda (env vars expVals)
     (let 
        (
          (lamdaPairList
            (Interpreter_filterLambda vars expVals)
          )
        )        
        (let 
           (
            (procEnv
                (Env_putPairList env lamdaPairList)
            )
           )
           (letrec 
             (
               (changeLambdaEnv!
                  (lambda (lambdaPairList)
                    (if (eqv? lambdaPairList '())
                      0
                      (let
                        (
                          (lambdaPair
                             (car lambdaPairList)
                          )
                          (lambdaPairRight 
                            (cadr 
                              (car lambdaPairList)
                            )
                          )
                        )
                        (cases ExpVal lambdaPairRight
                            (ExpVal_Lambda (proc)
                              (begin
                                (Proc_setEnv! proc procEnv)
                                (changeLambdaEnv!
                                    (cdr lambdaPairList) 
                                )
                              )
                            )
                            (else
                                (error 'Interpreter_procEnvBuild "what the fuck????")
                            )
                        ) 
                      )
                    )
                  )
               )
             )
             (changeLambdaEnv! lamdaPairList)
           )
        )
     )
    )
)



;Bounce ->ExpVal
(define tramoline
    (lambda (bounce)
        (if (ExpVal? bounce)
            bounce
            (tramoline (bounce))
        )
    )
)




;Program->ExpVal
(define InterpreterByCont_getProgramExpVal
    (lambda (program)
        (cases Program program
            (Program_Default (expr)
                (begin
                    (Sto_init!)
                    (Thread_init! 20)
                    ; (tramoline 
                        (InterpreterByCont_getExpVal 
                            expr 
                            (Env_createDefault) 
                            (Cont_createEndMain)
                            (TryCont_createEnd)
                        )
                    ; )
                )      
            )
            (else 
                (error 'InterpreterByCont_getProgramExpVal "Program type error")
            )
        )
    )
)

;Program -> SchemeValue
(define InterpreterByCont_runProgram
    (lambda (program)
        (ExpVal_getSchemeVal
            (InterpreterByCont_getProgramExpVal program)
        )
    )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cont ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-datatype Cont Cont?
    (Cont_EndMain)
    (Cont_EndSub)

    (Cont_Print
        (cont Cont?)
    )
    (Cont_Printn
        (cont Cont?)
    )

    (Cont_IsZero
        (cont Cont?)
    )
    
    (Cont_ConsRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_ConsEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_Car
        (cont Cont?)
    )
    (Cont_Cdr
        (cont Cont?)
    )

    (Cont_List
        (expVals (list-of ExpVal?))
        (exprs (list-of Expression?))
        (env Env?)
        (cont Cont?)
    )

    (Cont_Cond
        (conds (list-of Expression?))
        (acts  (list-of Expression?))
        (env Env?)
        (cont Cont?)
    )

    (Cont_IfElse
        (trueExp Expression?)
        (falseExp Expression?)
        (env Env?)
        (cont Cont?)
    )

    (Cont_Let
        (vars (list-of symbol?))
        (expVals (list-of ExpVal?))
        (exprs (list-of Expression?))
        (body Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_ProcCallBindValInit
        (exprs (list-of Expression?))
        (env Env?)
        (cont Cont?)
    )

    (Cont_ProcCallBindVal
        (proc ExpVal?)
        (exprs (list-of Expression?))
        (expVals (list-of ExpVal?))
        (env Env?)
        (cont Cont?)
    )
    (Cont_NewRef
        (cont Cont?)
    )
    (Cont_DeRef
        (cont Cont?)
    )
    (Cont_SetRefRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_SetRefEnd
        (ref number?)
        (cont Cont?)
    )
    (Cont_Begin
        (exprs (list-of Expression?))
        (env Env?)
        (cont Cont?)
    )

    (Cont_DiffRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_DiffEnd
        (left ExpVal?)
        (cont Cont?)
    )

    (Cont_MultRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_MultEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_AddRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_AddEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_DivRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_DivEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_LessRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_LessEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_LessEqualRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_LessEqualEnd
        (left ExpVal?)
        (cont Cont?)
    )

    (Cont_GreaterRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_GreaterEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_GreaterEqualRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_GreaterEqualEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_EqualRight
        (right Expression?)
        (env Env?)
        (cont Cont?)
    )
    (Cont_EqualEnd
        (left ExpVal?)
        (cont Cont?)
    )

    ;for enjecting a TryCont
    (Cont_TryCatch
        (cont Cont?)
    )

    (Cont_Thread
        (cont Cont?)
    )

    (Cont_Semaphore
        (cont Cont?)
    )

    (Cont_P
        (cont Cont?)
    )

    (Cont_V
        (cont Cont?)
    )
)

(define Cont_createEndMain
    (lambda ()
       (Cont_EndMain)
    )
)
(define Cont_createEndSub
    (lambda ()
       (Cont_EndSub)
    )
)


(define-datatype TryCont TryCont?
    (TryCont_End)
    (TryCont_Default
        (num number?)
        (expr Expression?)
        (env Env?)
        (cont Cont?)
        (tryCont TryCont?)
    )
)

(define TryCont_createEnd
    (lambda ()
        (TryCont_End)
    )
)


;Cont * expVal * TryCont ->expVal
(define Cont_continue
    (lambda (inCont tryCont expVal)
        ; (lambda ()
            (if (Thread_isTimeExpire)
                (begin
                    ; (display "newThread")
                    (Thread_enque!
                        (lambda ()
                            (Cont_continue inCont tryCont expVal)
                        )
                    )
                    (Thread_runNext!)
                )
                (begin
                    (Thread_timeDecrement!)
                    (cases Cont inCont
                        (Cont_EndMain ()
                            (begin
                                (Thread_setFinalAnswer! expVal)
                                (Thread_runNext!)
                            )
                        )
                        (Cont_EndSub ()
                            (begin
                                (Thread_runNext!)
                            )
                        )

                        (Cont_Print (cont)
                            (Cont_continue cont tryCont
                                (begin
                                    (display (ExpVal_getSchemeVal expVal))
                                    (ExpVal_Number 1)
                                )
                            )
                        )
                        (Cont_Printn (cont)
                            (Cont_continue cont tryCont
                                (begin
                                    (display (ExpVal_getSchemeVal expVal))
                                    (display #\newline)
                                    (ExpVal_Number 1)
                                ) 
                            )
                        )
                        (Cont_IsZero (cont)
                            (Cont_continue cont tryCont
                                (ExpVal_Bool
                                    (zero? 
                                        (ExpVal_getSchemeVal expVal)
                                    )
                                )
                            )
                        )
            
                        (Cont_ConsRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_ConsEnd expVal cont)
                                tryCont
                            )
                        )
            
                        (Cont_ConsEnd (left cont)
                            (Cont_continue cont tryCont
                                (ExpVal_LinkList left expVal)
                            )
                        )
            
                        (Cont_Car (cont)
                            (cases ExpVal expVal
                                (ExpVal_LinkList (left right)
                                    (Cont_continue cont tryCont
                                        left
                                    )
                                )
                                (else
                                    (error 'Cont_continue "can not car a non-list")
                                )
                            )
                        )
            
                        (Cont_Cdr (cont)
                            (cases ExpVal expVal
                                (ExpVal_LinkList (left right)
                                    (Cont_continue cont tryCont
                                        right
                                    )
                                )
                                (else
                                    (error 'Cont_continue "can not car a non-list")
                                )
                            )
                        )
            
                        (Cont_List (expVals exprs env cont)
                            (if (eqv? exprs '())
                                (Cont_continue cont tryCont
                                    (ExpVal_buildLinkList
                                        (append expVals (list expVal))
                                    )
                                )
                                (InterpreterByCont_getExpVal
                                    (car exprs)
                                    env
                                    (Cont_List 
                                        (append expVals (list expVal))
                                        (cdr exprs)
                                        env
                                        cont
                                    )
                                    tryCont
                                )
                            )
                        )
            
                        (Cont_Cond (conds acts env cont)
                            (if (ExpVal_getSchemeVal expVal)
                                (InterpreterByCont_getExpVal
                                    (car acts)
                                    env
                                    cont
                                    tryCont
                                )
                                (if (eqv? conds '())
                                    (error 'Cont_Cond "there is not a default act")
                                    (InterpreterByCont_getExpVal
                                        (car conds)
                                        env
                                        (Cont_Cond
                                            (cdr conds)
                                            (cdr acts)
                                            env
                                            cont
                                        )
                                        tryCont
                                    )     
                                )        
                            )
                        )
            
            
            
                        (Cont_Let (vars expVals exprs body env cont)
                            (if (eqv? exprs '())
                               (InterpreterByCont_getExpVal
                                 body
                                 (InterpreterByCont_letBind env vars 
                                    (append expVals (list expVal))
                                 )
                                 cont
                                 tryCont
                               )
                               (InterpreterByCont_getExpVal
                                  (car exprs)
                                  env
                                  (Cont_Let
                                     vars
                                     (append expVals (list expVal))
                                     (cdr exprs)
                                     body
                                     env
                                     cont        
                                  )
                                  tryCont   
                               )
                            )
                        )
            
                        (Cont_ProcCallBindValInit (exprs env cont)
                            (if (eqv? exprs '())
                                (InterpreterByCont_getProcExpVal
                                    expVal
                                    '()
                                    cont
                                    tryCont
                                )
                                (InterpreterByCont_getExpVal
                                    (car exprs)
                                    env
                                    (Cont_ProcCallBindVal
                                        expVal
                                        (cdr exprs)
                                        '()
                                        env
                                        cont
                                    )
                                    tryCont
                                )
                            )
                        )
            
                        (Cont_ProcCallBindVal (proc exprs expVals env cont)
                            (if (eqv? exprs '())
                                (InterpreterByCont_getProcExpVal
                                    proc
                                    (append expVals (list expVal))
                                    cont
                                    tryCont
                                )
                                (InterpreterByCont_getExpVal
                                    (car exprs)
                                    env
                                    (Cont_ProcCallBindVal
                                        proc
                                        (cdr exprs)
                                        (append expVals (list expVal))
                                        env
                                        cont
                                        tryCont
                                    )
                                )       
                            )
                        )
            
                        (Cont_NewRef (cont)
                            (Cont_continue cont
                                (ExpVal_Ref
                                    (Sto_newRef! expVal)
                                )         
                            )
                        )
            
                        (Cont_DeRef (cont)
                            (cases ExpVal expVal
                                (ExpVal_Ref (ref)
                                    (Cont_continue cont tryCont
                                        (Sto_deRef ref)
                                    )
                                )
                                (else
                                    (error 'Cont_DeRef "can't deref a non-ref")
                                )
                            )
                        )
            
                        (Cont_SetRefRight (right env cont)
                            (cases ExpVal expVal
                                (ExpVal_Ref (ref)
                                    (InterpreterByCont_getExpVal
                                        right
                                        env
                                        (Cont_SetRefEnd 
                                            ref
                                            cont
                                        )
                                        tryCont
                                    )
                                )
                                (else
                                    (error 'InterpreterByCont_getExpVal "can't set! a non-ref")
                                )    
                            )
                        )
            
                        (Cont_SetRefEnd (ref cont)
                            (Cont_continue cont tryCont
                                (begin 
                                    (Sto_setRef! ref expVal)
                                    (ExpVal_Number 1)
                                )
                            )
                        )
            
                        (Cont_Begin (exprs env cont)
                            (if (eqv? exprs '())
                                (Cont_continue cont tryCont
                                    expVal
                                )
                                (InterpreterByCont_getExpVal
                                    (car exprs)
                                    env
                                    (Cont_Begin 
                                        (cdr exprs)
                                        env
                                        cont
                                    )
                                    tryCont
                                )
                            )
                            
                        )
        
                        (Cont_DiffRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_DiffEnd expVal cont)
                                tryCont
                            )
                        )
                        
                        (Cont_DiffEnd (left cont)
                            (Cont_binaryOpEndContinue - ExpVal_Number left expVal cont tryCont)
                        )
            
                        (Cont_MultRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_MultEnd expVal cont)
                            )
                        )
            
                        (Cont_MultEnd (left cont)
                            (Cont_binaryOpEndContinue * ExpVal_Number left expVal cont tryCont)
                        )
            
                        (Cont_AddRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_AddEnd expVal cont)
                            )
                        )
            
                        (Cont_AddEnd (left cont)
                            (Cont_binaryOpEndContinue + ExpVal_Number left expVal cont tryCont)
                        )
            
                        (Cont_DivRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_DivEnd expVal cont)
                            )
                        )
            
                        (Cont_DivEnd (left cont)
                            (if (zero? (ExpVal_getSchemeVal expVal))
                                (error 'Cont_continue "The divisor can not be 0")
                                (Cont_binaryOpEndContinue / ExpVal_Number left expVal cont tryCont)
                            )
                        )
            
                        
                        (Cont_LessRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_LessEnd expVal cont)
                            )
                        )
            
                        (Cont_LessEnd (left cont)
                            (Cont_binaryOpEndContinue < ExpVal_Bool left expVal cont tryCont)
                        )
            
                        (Cont_LessEqualRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_LessEqualEnd expVal cont)
                                tryCont 
                            )
                        )
            
                        (Cont_LessEqualEnd (left cont)
                            (Cont_binaryOpEndContinue <= ExpVal_Bool left expVal cont tryCont) 
                        )
            
                        (Cont_GreaterRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_GreaterEnd expVal cont)
                                tryCont
                            )
                        )
            
                        (Cont_GreaterEnd (left cont)
                            (Cont_binaryOpEndContinue > ExpVal_Bool left expVal cont tryCont)
                        )
                        
                        (Cont_GreaterEqualRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_GreaterEqualEnd expVal cont)
                                tryCont
                            )
                        )
            
                        (Cont_GreaterEqualEnd (left cont)
                            (Cont_binaryOpEndContinue >= ExpVal_Bool left expVal cont tryCont)
                        )
                        
                        (Cont_EqualRight (right env cont)
                            (InterpreterByCont_getExpVal
                                right
                                env
                                (Cont_EqualEnd expVal cont)
                                tryCont
                            )
                        )
            
                        (Cont_EqualEnd (left cont)
                            (Cont_binaryOpEndContinue =  ExpVal_Bool left expVal cont tryCont)
                        )
            
                        (Cont_IfElse (trueExp falseExp env cont)
                            (if (ExpVal_getSchemeVal expVal)
                               (InterpreterByCont_getExpVal trueExp env cont tryCont)
                               (InterpreterByCont_getExpVal falseExp env cont tryCont)
                            )
                        )
                        
                        ;enject a tryCont
                        (Cont_TryCatch (cont)
                            (cases TryCont tryCont
                                (TryCont_Default (num expr env saveCont saveTryCont)
                                    (Cont_continue cont saveTryCont expVal)    
                                )
                                (else
                                    (error 'Cont_TryCont "WTF????")
                                )
                            )
                        )

                        (Cont_Thread (cont)
                            (cases ExpVal expVal
                                (ExpVal_Lambda (proc)
                                    (begin
                                        (Thread_enque!
                                            (lambda ()
                                                (InterpreterByCont_getProcExpVal 
                                                    expVal 
                                                    '() 
                                                    (Cont_createEndSub) 
                                                    (TryCont_createEnd)
                                                ) 
                                            )
                                        )
                                        (Cont_continue cont tryCont
                                            (ExpVal_Number 1)
                                        )
                                    )
                                )
                                (else
                                    (error 'Cont_Thread  "can not create a thread with a non-lambda")
                                )
                            )
                        )

                        (Cont_Semaphore (cont)
                            (cases ExpVal expVal
                                (ExpVal_Number (num)
                                    (Cont_continue cont tryCont
                                        (ExpVal_Semaphore
                                            (Semaphore_create num)
                                        )
                                    )
                                )
                                (else
                                    (error 'Cont_Semaphore  "can not create a Semaphored with a non-number")
                                )
                            )
                        )

                        (Cont_P (cont)
                            (cases ExpVal expVal
                                (ExpVal_Semaphore (semaphore)
                                    (Semaphore_p! semaphore
                                        (lambda ()
                                            (Cont_continue cont tryCont
                                                (ExpVal_Number 1)
                                            )
                                        )
                                    )
                                )
                                (else
                                    (error 'Cont_Semaphore  "can not create a Semaphored with a non-number")
                                )
                            )
                        )

                        (Cont_V (cont)
                            (cases ExpVal expVal
                                (ExpVal_Semaphore (semaphore)
                                    (begin
                                        (Semaphore_v! semaphore)
                                        (Cont_continue cont tryCont
                                            (ExpVal_Number 1)
                                        )
                                    )
                                )
                                (else
                                    (error 'Cont_Semaphore  "can not create a Semaphored with a non-number")
                                )
                            )
                        )
            
                        (else
                            (error 'Cont_continue "WTF continuation????")
                        )
                    )
                )  
            )
        ; )
    )
)



(define TryCont_exceptionHandle
    (lambda (contIn numIn)
        (cases TryCont contIn
            (TryCont_Default (num expr env cont tryCont)
                (if (= numIn num)
                    (InterpreterByCont_getExpVal
                        expr
                        env
                        cont
                        tryCont
                    )
                    (TryCont_exceptionHandle tryCont numIn)
                )
            )
            (else
                (begin
                    (eopl:printf "Exception raise : ~s" numIn)
                    (Thread_runNext!)
                )        
            )
        )
    )
)

(define Cont_binaryOpEndContinue
    (lambda (op type left right cont tryCont)
        (let (
              (nextVal
                (type
                    (op
                       (ExpVal_getSchemeVal left)
                       (ExpVal_getSchemeVal right)
                    )
                )      
              )
            )
            (Cont_continue cont tryCont nextVal)
        )
    )
)

