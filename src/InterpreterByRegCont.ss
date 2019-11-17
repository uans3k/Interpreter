(load "./eopl.ss")
(load "./Expression.ss")
(load "./Env.ss")
(load "./ExpVal.ss")
(load "./Sto.ss")

(define InterpreterByRegCont_env 'undefine)
(define InterpreterByRegCont_val 'undefine)
(define InterpreterByRegCont_expr 'undefine)
(define InterpreterByRegCont_cont 'undefine)




;Expression * Env * Cont -> ExpVal
(define InterpreterByRegCont_getExpVal
    (lambda ()
        (cases Expression InterpreterByRegCont_expr

            (Expression_Const (num)
                (begin
                    (set! InterpreterByRegCont_val (ExpVal_Number num))
                    (Cont_continue)
                )
            )

            (Expression_Default ()
                (begin
                    (set! InterpreterByRegCont_val (ExpVal_Bool #t))
                    (Cont_continue)
                )
            )

            (Expression_True  ()
                (begin
                    (set! InterpreterByRegCont_val (ExpVal_Bool #t))
                    (Cont_continue)
                )   
            )

            (Expression_False ()
                (begin
                    (set! InterpreterByRegCont_val (ExpVal_Bool #f))
                    (Cont_continue)
                )   
            )

            (Expression_Diff (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_DiffRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                )  
            )
            
            (Expression_Add (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_AddRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                )  
            )

            (Expression_Mult (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_MultRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                )  
            )
            
            (Expression_Div (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_MultRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                ) 
            )
            

            (Expression_IsZero (expr)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_IsZero cont))
                    (set! InterpreterByRegCont_expr expr)
                    (InterpreterByRegCont_getExpVal)
                ) 
            )
            
            (Expression_Greater (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_GreaterRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                ) 
            )

            (Expression_GreaterEqual (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_GreaterEqualRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                )
            )

             
            (Expression_Equal (left right)
                (begin
                    (set! InterpreterByRegCont_cont (Cont_EqualRight right env cont))
                    (set! InterpreterByRegCont_expr left)
                    (InterpreterByRegCont_getExpVal)
                )
            )

             
            (Expression_Less (left right)
               (InterpreterByRegCont_getExpVal
                  left
                  env
                  (Cont_LessRight right env cont)
               )
            )
              
            (Expression_LessEqual (left right)
               (InterpreterByRegCont_getExpVal
                  left
                  env
                  (Cont_LessEqualRight right env cont)
               )
            )
            

            (Expression_Cons (left right)
               (InterpreterByRegCont_getExpVal
                  left
                  env
                  (Cont_ConsRight right env cont)
               )
            )

            (Expression_Car (expr)
                (InterpreterByRegCont_getExpVal expr env
                    (Cont_Car cont)
                )
            )

            (Expression_Cdr (expr)
                (InterpreterByRegCont_getExpVal expr env
                    (Cont_Cdr cont)
                )
            )

            (Expression_EmptyList ()
                (Cont_continue
                    (ExpVal_EmptyListVal)
                    cont
                )
            )
            
            ;List<Expression>->ExpVal_List<ExpVal>
            (Expression_List (exprs)
                (InterpreterByRegCont_getExpVal
                    (car exprs)
                    env
                    (Cont_List
                        '()
                        (cdr exprs)
                        env
                        cont
                    )
                ) 
            )

            (Expression_Cond (conds acts)
                (InterpreterByRegCont_getExpVal 
                    (car conds)
                    env
                    (Cont_Cond
                        (cdr conds)
                        acts
                        env
                        cont
                    )
                )
            )



            (Expression_IfElse (predExp trueExp falseExp)
                (InterpreterByRegCont_getExpVal
                    predExp
                    env
                    (Cont_IfElse trueExp falseExp env cont)
                )
            )
            
            (Expression_Print (expr)
                (InterpreterByRegCont_getExpVal
                    expr
                    env
                    (Cont_Print cont)
                )
            )
            
            (Expression_Printn (expr)
                (InterpreterByRegCont_getExpVal
                    expr
                    env
                    (Cont_Printn cont)
                )
            )
            

            (Expression_Var (var)
                (Cont_continue cont
                    (Env_get env var)
                )
                
            )

            (Expression_Let (vars exprs body)
               (InterpreterByRegCont_getExpVal
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
               )
            )

            (Expression_Lambda (vars body)
               (Cont_continue cont
                  (ExpVal_Lambda
                    (Proc_create vars body env)
                  )
               ) 
            )

            (Expression_ProcCall (expr exprs)
                (InterpreterByRegCont_getExpVal
                    expr
                    env
                    (Cont_ProcCallBindValInit
                        exprs
                        env
                        cont
                    )
                )
            )
            
            (Expression_NewRef (expr)
                (InterpreterByRegCont_getExpVal 
                    expr 
                    env
                    (Cont_NewRef cont)
                )
            )
            
            (Expression_DeRef (expr)
                (InterpreterByRegCont_getExpVal 
                    expr 
                    env
                    (Cont_DeRef cont)
                )
            )
            
            (Expression_SetRef (left right)
                (InterpreterByRegCont_getExpVal 
                    left 
                    env
                    (Cont_SetRefRight right env cont)
                )
            )

            (Expression_Begin (exprs)
                (InterpreterByRegCont_getExpVal
                    (car exprs)
                    env
                    (Cont_Begin
                        (cdr exprs)
                        env
                        cont
                    )
                )
            )

            (else 
                (error 'InterpreterByRegCont_getExpVal "Expression type error")
            )
        )
      )
)



(define InterpreterByCont_getProcExpVal
    (lambda (expValProc expVals cont)
      (cases ExpVal expValProc
         (ExpVal_Lambda (proc)
            (InterpreterByRegCont_getExpVal 
               (Proc_getBody proc)
               (Env_putList
                  (Proc_getEnv proc)
                  (Proc_getVars proc)
                  expVals
               )
               cont
            )
         )
         (else
           (error 'InterpreterByRegCont_getExpVal "it is not a proc")
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



;Program->ExpVal
(define InterpreterByCont_getProgramExpVal
    (lambda (program)
        (cases Program program
            (Program_Default (expr)
                (Sto_init)
                (InterpreterByRegCont_getExpVal 
                    expr 
                    (Env_createDefault) 
                    (Cont_createEnd)
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
    (Cont_End)

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
)

(define Cont_createEnd
    (lambda ()
       (Cont_End)
    )
)

;Cont * expVal ->expVal
(define Cont_continue
    (lambda (cont expVal)
        (cases Cont cont
            (Cont_End ()
                (display "End of Program!")
                expVal
            )
            (Cont_Print (cont)
                (Cont_continue cont
                    (begin
                        (display (ExpVal_getSchemeVal expVal))
                        (ExpVal_Number 1)
                    )
                )
            )
            (Cont_Printn (cont)
                (Cont_continue cont 
                    (begin
                        (display (ExpVal_getSchemeVal expVal))
                        (display #\newline)
                        (ExpVal_Number 1)
                    ) 
                )
            )
            (Cont_IsZero (cont)
                (Cont_continue cont
                    (ExpVal_Bool
                        (zero? 
                            (ExpVal_getSchemeVal expVal)
                        )
                    )
                )
            )

            (Cont_ConsRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_ConsEnd expVal cont)
                )
            )

            (Cont_ConsEnd (left cont)
                (Cont_continue cont
                    (ExpVal_LinkList left expVal)
                )
            )

            (Cont_Car (cont)
                (cases ExpVal expVal
                    (ExpVal_LinkList (left right)
                        (Cont_continue cont
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
                        (Cont_continue cont
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
                    (Cont_continue cont
                        (ExpVal_buildLinkList
                            (append expVals (list expVal))
                        )
                    )
                    (InterpreterByRegCont_getExpVal
                        (car exprs)
                        env
                        (Cont_List 
                            (append expVals (list expVal))
                            (cdr exprs)
                            env
                            cont
                        )
                    )
                )
            )

            (Cont_Cond (conds acts env cont)
                (if (ExpVal_getSchemeVal expVal)
                    (InterpreterByRegCont_getExpVal
                        (car acts)
                        env
                        cont
                    )
                    (if (eqv? conds '())
                        (error 'Cont_Cond "there is not a default act")
                        (InterpreterByRegCont_getExpVal
                            (car conds)
                            env
                            (Cont_Cond
                                (cdr conds)
                                (cdr acts)
                                env
                                cont
                            )
                        )     
                    )        
                )
            )



            (Cont_Let (vars expVals exprs body env cont)
                (if (eqv? exprs '())
                   (InterpreterByRegCont_getExpVal
                     body
                     (InterpreterByCont_letBind env vars 
                        (append expVals (list expVal))
                     )
                     cont
                   )
                   (InterpreterByRegCont_getExpVal
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
                   )
                )
            )

            (Cont_ProcCallBindValInit (exprs env cont)
                (if (eqv? exprs '())
                    (InterpreterByCont_getProcExpVal
                        expVal
                        '()
                        cont
                    )
                    (InterpreterByRegCont_getExpVal
                        (car exprs)
                        env
                        (Cont_ProcCallBindVal
                            expVal
                            (cdr exprs)
                            '()
                            env
                            cont
                        )
                    )
                )
            )

            (Cont_ProcCallBindVal (proc exprs expVals env cont)
                (if (eqv? exprs '())
                    (InterpreterByCont_getProcExpVal
                        proc
                        (append expVals (list expVal))
                        cont
                    )
                    (InterpreterByRegCont_getExpVal
                        (car exprs)
                        env
                        (Cont_ProcCallBindVal
                            proc
                            (cdr exprs)
                            (append expVals (list expVal))
                            env
                            cont
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
                        (Cont_continue cont
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
                        (InterpreterByRegCont_getExpVal
                            right
                            env
                            (Cont_SetRefEnd 
                                ref
                                cont
                            )
                        )
                    )
                    (else
                        (error 'InterpreterByRegCont_getExpVal "can't set! a non-ref")
                    )    
                )
            )

            (Cont_SetRefEnd (ref cont)
                (Cont_continue cont
                    (begin 
                        (Sto_setRef! ref expVal)
                        (ExpVal_Number 1)
                    )
                )
            )

            (Cont_Begin (exprs env cont)
                (if (eqv? exprs '())
                    (Cont_continue cont
                        expVal
                    )
                    (InterpreterByRegCont_getExpVal
                        (car exprs)
                        env
                        (Cont_Begin 
                            (cdr exprs)
                            env
                            cont
                        )
                    )
                )
                
            )
            (Cont_DiffRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_DiffEnd expVal cont)
                )
            )
            
            (Cont_DiffEnd (left cont)
                (Cont_binaryOpEndContinue - ExpVal_Number left expVal cont)
            )

            (Cont_MultRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_MultEnd expVal cont)
                )
            )

            (Cont_MultEnd (left cont)
                (Cont_binaryOpEndContinue * ExpVal_Number left expVal cont)
            )

            (Cont_AddRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_AddEnd expVal cont)
                )
            )

            (Cont_AddEnd (left cont)
                (Cont_binaryOpEndContinue + ExpVal_Number left expVal cont)
            )

            (Cont_DivRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_DivEnd expVal cont)
                )
            )

            (Cont_DivEnd (left cont)
                (if (zero? (ExpVal_getSchemeVal expVal))
                    (error 'Cont_continue "The divisor can not be 0")
                    (Cont_binaryOpEndContinue / ExpVal_Number left expVal cont)
                )
            )

            
            (Cont_LessRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_LessEnd expVal cont)
                )
            )

            (Cont_LessEnd (left cont)
                (Cont_binaryOpEndContinue < ExpVal_Bool left expVal cont)
            )

            (Cont_LessEqualRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_LessEqualEnd expVal cont)
                )
            )

            (Cont_LessEqualEnd (left cont)
                (Cont_binaryOpEndContinue <= ExpVal_Bool left expVal cont)
            )

            (Cont_GreaterRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_GreaterEnd expVal cont)
                )
            )

            (Cont_GreaterEnd (left cont)
                (Cont_binaryOpEndContinue > ExpVal_Bool left expVal cont)
            )
            
            (Cont_GreaterEqualRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_GreaterEqualEnd expVal cont)
                )
            )

            (Cont_GreaterEqualEnd (left cont)
                (Cont_binaryOpEndContinue >= ExpVal_Bool left expVal cont)
            )
            
            (Cont_EqualRight (right env cont)
                (InterpreterByRegCont_getExpVal
                    right
                    env
                    (Cont_EqualEnd expVal cont)
                )
            )

            (Cont_EqualEnd (left cont)
                (Cont_binaryOpEndContinue =  ExpVal_Bool left expVal cont)
            )

            (Cont_IfElse (trueExp falseExp env cont)
                (if (ExpVal_getSchemeVal expVal)
                   (InterpreterByRegCont_getExpVal trueExp env cont)
                   (InterpreterByRegCont_getExpVal falseExp env cont)
                )
            )
        )
      )
)


(define Cont_binaryOpEndContinue
    (lambda (op type left right cont)
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
            (Cont_continue cont nextVal)
        )
    )
)

