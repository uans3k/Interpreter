(load "./eopl.ss")
(load "./Expression.ss")
(load "./Env.ss")
(load "./ExpVal.ss")
(load "./Sto.ss")

;Expression * Env -> ExpVal
(define Interpreter_getExpVal
    (lambda (expression env)
        (cases Expression expression

            (Expression_Const (num)
                (ExpVal_Number num)
            )

            (Expression_Default ()
                (ExpVal_Boolean #t)
            )

            (Expression_True ()
                (ExpVal_Boolean #t)
            )

            (Expression_False ()
                (ExpVal_Boolean #f)
            )
            
            (Expression_Diff (left right)
                (ExpVal_Number 
                    (-
                      (ExpVal_getSchemeVal 
                        (Interpreter_getExpVal left env)
                      )
                      (ExpVal_getSchemeVal 
                        (Interpreter_getExpVal right env)
                      )
                    )
                )
            )
            
            (Expression_Add (left right)
                (ExpVal_Number 
                     (+
                        (ExpVal_getSchemeVal 
                            (Interpreter_getExpVal left env)
                        )
                        (ExpVal_getSchemeVal 
                            (Interpreter_getExpVal right env)
                        )
                     )
                )
            )

            (Expression_Mult (left right)
                (ExpVal_Number 
                     (*
                        (ExpVal_getSchemeVal 
                            (Interpreter_getExpVal left env)
                        )
                        (ExpVal_getSchemeVal 
                            (Interpreter_getExpVal right env)
                        )
                     )
                )
            )
            
            (Expression_Div (left right)
                (ExpVal_Number
                    (let 
                        (
                          (rightVal 
                              (ExpVal_getSchemeVal 
                                  (Interpreter_getExpVal right env)
                              ) 
                          )
                        )
                        (
                          if (zero? rightVal)
                             (error 'Interpreter_getExpVal "the right of Div must not be 0")
                             (/
                                (ExpVal_getSchemeVal 
                                    (Interpreter_getExpVal left env)
                                )
                                rightVal   
                             )
                            
                        )

                    )
                )
            )


            (Expression_IsZero (expr)
                (let (
                      (schemeVal (ExpVal_getSchemeVal (Interpreter_getExpVal expr env)))
                     )
                     (if (zero? schemeVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
            
            (Expression_Greater (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (Interpreter_getExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (Interpreter_getExpVal right env)))
                     )
                     (if (> leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

            (Expression_GreaterEqual (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (Interpreter_getExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (Interpreter_getExpVal right env)))
                     )
                     (if (>= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

             
            (Expression_Equal (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (Interpreter_getExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (Interpreter_getExpVal right env)))
                     )
                     (if (= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

             
            (Expression_Less (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (Interpreter_getExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (Interpreter_getExpVal right env)))
                     )
                     (if (< leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
              
            (Expression_LessEqual (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (Interpreter_getExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (Interpreter_getExpVal right env)))
                     )
                     (if (<= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
            

            (Expression_Cons (left right)
                (let (
                      (leftVal (Interpreter_getExpVal left env))
                      (rightVal (Interpreter_getExpVal right env))
                     )
                     (ExpVal_LinkList leftVal rightVal)
                )
            )

            (Expression_Car (expr)
                (ExpVal_car (Interpreter_getExpVal expr env)
                )
            )

            (Expression_Cdr (expr)
                (ExpVal_cdr (Interpreter_getExpVal expr env)
                )
            )

            (Expression_EmptyList ()
                (ExpVal_EmptyListVal)
            )
            
            ;List<Expression>->ExpVal_List<ExpVal>
            (Expression_List (args)
                (letrec (
                          (getLinkList
                              (lambda (exprs)
                                 (if (eqv? '() exprs)
                                     (ExpVal_EmptyListVal)
                                     (ExpVal_LinkList 
                                        (Interpreter_getExpVal (car exprs) env) 
                                        (getLinkList (cdr exprs))
                                     )
                                 )
                              )   
                          )
                        )
                        (getLinkList args)
                )
            )

            (Expression_Cond (conds acts)
                (Interpreter_getCondExpVal conds acts env)
            )



            (Expression_IfElse (predExp trueExp falseExp)
                (if (ExpVal_getSchemeVal (Interpreter_getExpVal predExp env))
                   (Interpreter_getExpVal trueExp env)
                   (Interpreter_getExpVal falseExp env)
                )
            )
            
            (Expression_Print (expr)
                (let (
                       (val 
                         (ExpVal_getSchemeVal
                            (Interpreter_getExpVal  expr env)
                         )
                       )
                     )
                     (begin 
                        (display val)
                        (ExpVal_Number 1)
                     )
                )
            )
            
            (Expression_Printn (expr)
                 (let (
                       (val 
                         (ExpVal_getSchemeVal
                            (Interpreter_getExpVal expr env)
                         )
                       )
                     )
                     (begin 
                        (display val)
                        (display #\newline)
                        (ExpVal_Number 1)
                     )
                )
            )
            

            (Expression_Var (var)
                (Env_get env var)
            )
            
         

            (Expression_Let (vars exprs body)
               (Interpreter_getExpVal
                    body
                    (Interpreter_letBind env vars exprs)
               )
            )

            ; (Expression_Proc (id vars body)
            ;   (ExpVal_Proc id vars body 
            ;      (Env_put env 
            ;         id
            ;         (ExpVal_Proc id vars body env)
            ;      )
            ;   )
            ; )

            (Expression_Lambda (vars body)
               (ExpVal_Lambda 
                  (Proc_create vars body env)
               ) 
            )

            (Expression_ProcCall (expr exprs)
                (let 
                    (
                        (expvalProc (Interpreter_getExpVal expr env))
                    )
                    (Interpreter_getProcExpVal expvalProc exprs env)
                )        
            )
            
            (Expression_NewRef (expr)
               (ExpVal_Ref
                  (Sto_newRef! 
                    (Interpreter_getExpVal expr env)
                  )
               )
            )
            
            (Expression_DeRef (expr)
                (cases ExpVal (Interpreter_getExpVal expr env)
                    (ExpVal_Ref (ref)
                        (Sto_deRef ref)
                    )
                    (else
                        (error 'Interpreter_getExpVal "can't deref a non-ref")
                    )
                )
            )
            
            (Expression_SetRef (left right)
                (cases ExpVal (Interpreter_getExpVal left env)
                    (ExpVal_Ref (ref)
                        (Sto_setRef! ref
                            (Interpreter_getExpVal right env)
                        )
                        (ExpVal_Number 1)
                    )
                    (else
                        (error 'Interpreter_getExpVal "can't set! a non-ref")
                    )    
                )
            )

            (Expression_Begin (exprs)
                (Interpreter_getBeginExpVal exprs env)
            )

           

            (else 
                (error 'Interpreter_getExpVal "Expression type error")
            )
        )
    )
)



(define Interpreter_getBeginExpVal
    (lambda (exprs env)
       (letrec (
              (getBeginExpValInner
                   (lambda (expr exprs)
                     (let (
                            (expVal 
                                (Interpreter_getExpVal expr env)
                            )
                          )
                          (if (eqv? exprs '())
                               expVal
                               (getBeginExpValInner (car exprs) (cdr exprs))
                          )
                     )
                   )
              )
            )
            (getBeginExpValInner
                (car exprs) 
                (cdr exprs)
            )
       )
    )
)



;ExpVal_Lambda * list<Expression>->ExpVal
(define Interpreter_getProcExpVal
   (lambda (expValProc exprs env)
     (cases ExpVal expValProc
        (ExpVal_Lambda (proc)
           (Interpreter_getExpVal 
              (Proc_getBody proc)
              (Interpreter_callBind 
                env 
                (Proc_getEnv proc)
                (Proc_getVars proc) 
                exprs
              )
           )
        )
        (else
          (error 'Interpreter_getExpVal "it is not a proc")
        )
     )
   )
)

(define Interpreter_callBind
    (lambda (env saveEnv vars exprs)
       (let (
              (vals (map (Interpreter_exp2expVal env) exprs))
            )
            (Env_putList saveEnv vars vals)
       )
    )
)

;Env * list<symbol> * list<Expression> -> Env
(define Interpreter_letBind
    (lambda (env vars exprs)
        (let (
                (expVals 
                    (map
                        (lambda (expr)
                            (Interpreter_getExpVal expr env)
                        )
                        exprs
                    )
                )
             )
             (begin 
                (Interpreter_procEnvBuild! env vars expVals)
                (Env_putList env vars expVals)        
             )
        )
    )
)

(define Interpreter_procEnvBuild!
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
                      1
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

; (define Interpreter_letBindInner
;     (lambda (env procEnv vars expVals)
;       (if (eqv? vars '())
;         env
;         (let (
;                 (headVar (car vars))
;                 (headVal (car expVals))
;                 (tailVars (cdr vars))
;                 (tailExpVals (cdr expVals))
;              )
;              (cases ExpVal headVal
;                   (ExpVal_Lambda (varsInner body saveEnv)
;                       (Interpreter_letBindInner
;                          (Env_put env 
;                             headVar
;                             (ExpVal_Lambda varsInner body procEnv)
;                          )
;                          procEnv
;                          tailVars
;                          tailExpVals
;                       )
;                   )
;                   (else
;                       (Interpreter_letBindInner
;                          (Env_put env 
;                             headVar
;                             headVal
;                          )
;                          procEnv
;                          tailVars
;                          tailExpVals
;                       )
;                   )
;              )
;         )
;       )
;     )
; )


;list<symbol> * list<ExpVal> -> list(Pair<symobol,ExpVal_Lambda>)
(define Interpreter_filterLambda
    (lambda (vars expVals)
        (if (eqv? vars '())
            '()
            (let (
                    (headVar (car vars))
                    (headExpVal (car expVals))
                    (tailVars (cdr vars))
                    (tailExpVals (cdr expVals))
                 )
                 (cases ExpVal headExpVal
                    (ExpVal_Lambda (proc)
                        (cons 
                           (list headVar headExpVal)
                           (Interpreter_filterLambda tailVars tailExpVals)
                        )
                    )
                    (else
                        (Interpreter_filterLambda tailVars tailExpVals)
                    )
                )
            )
        )
    )
)





; not support recursion
; (define Interpreter_letBind
;     (lambda (env vars exprs)
;         (if (eqv? vars '())
;             env
;             (let (
;                   (var (car vars))
;                   (expr (car exprs))
;                   (varsTail (cdr vars))
;                   (exprsTail (cdr exprs))
;                  )
;                  (Interpreter_letBind
;                     (Env_put env var 
;                         (Interpreter_getExpVal expr env)
;                     )
;                     varsTail
;                     exprsTail
;                  )
;             )
;         ) 
;     )
; )

(define Interpreter_exp2expVal
    (lambda (env)
        (lambda (expr)
            (Interpreter_getExpVal expr env)
        )
    )
)

(define Interpreter_getCondExpVal
    (lambda (conds acts env)
        (let (
                (condExp (car conds))
                (actExp (car acts))
             )
             (if (eqv? condExp '())
                  (error 'Interpreter_getExpVal "there is not a defaut cond")
                  (if (ExpVal_getSchemeVal (Interpreter_getExpVal condExp env))
                        (Interpreter_getExpVal actExp env)
                        (Interpreter_getCondExpVal (cdr conds) (cdr acts) env)
                  )                  
             )
        ) 
    )
)



;Program->ExpVal
(define Interpreter_getProgramExpVal
    (lambda (program)
        (cases Program program
            (Program_Default (expr)
                (Sto_init)
                (Interpreter_getExpVal expr (Env_createDefault))
            )
            (else (error 'Interpreter_getProgramExpVal "Program type error"))
        )
    )
)

;Program -> SchemeValue
(define Interpreter_runProgram
    (lambda (program)
        (ExpVal_getSchemeVal
            (Interpreter_getProgramExpVal program)
        )
    )
)

