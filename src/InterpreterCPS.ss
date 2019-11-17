(load "./eopl.ss")
(load "./ExprSimpleCPS.ss")

;ExprTailForm -> ExpVal
(define InterpreterCPS_getTailFormExpVal
    (lambda (inExpr env)
        (cases ExpExprTailFrom inExpr
            (ExprTailForm_Simple (expr)
                (InterpreterCPS_getSimpleExpVal expr)
            )
            (ExprTailForm_Call (rator rands)
                (let 
                    (
                        (expvalProc (InterpreterCPS_getSimpleExpVal rator env))
                    )
                    (Interpreter_getProcExpVal expvalProc exprs env)
                )        
            )

            (ExprTailForm_Let (vars exprs body)
                (InterpreterCPS_getTailFormExpVal
                    body
                    (InterpreterCPS_letBind env vars exprs)
                )
            )

            (ExprTailForm_Print (expr cont)
                
            )
            
            (Expression_Printn (expr cont)
                
            )
        )
    )
)
;ExprSimple->ExpVal
(define InterpreterCPS_getSimpleExpVal
    (lambda (inExpr env)
        (cases ExprSimple inExpr
            (ExprSimple_Const (num)
                (ExpVal_Number num)
            )
            (ExprSimple_True ()
                (ExpVal_Bool #t)
            )
              
            (ExprSimple_False ()
                (ExpVal_Boolean #f)
            )
            
            (ExprSimple_Diff (left right)
                (ExpVal_Number 
                    (-
                      (ExpVal_getSchemeVal 
                        (InterpreterCPS_getSimpleExpVal left env)
                      )
                      (ExpVal_getSchemeVal 
                        (InterpreterCPS_getSimpleExpVal right env)
                      )
                    )
                )
            )
            
            (ExprSimple_Add (left right)
                (ExpVal_Number 
                     (+
                        (ExpVal_getSchemeVal 
                            (InterpreterCPS_getSimpleExpVal left env)
                          )
                        (ExpVal_getSchemeVal 
                            (InterpreterCPS_getSimpleExpVal right env)
                        )
                     )
                )
            )

            (ExprSimple_Mult (left right)
                (ExpVal_Number 
                     (*
                        (ExpVal_getSchemeVal 
                            (InterpreterCPS_getSimpleExpVal left env)
                          )
                        (ExpVal_getSchemeVal 
                            (InterpreterCPS_getSimpleExpVal right env)
                        )
                     )
                )
            )
            
            (ExprSimple_Div (left right)
                (ExpVal_Number
                    (let 
                        (
                          (rightVal 
                              (ExpVal_getSchemeVal 
                                  (InterpreterCPS_getSimpleExpVal right env)
                              ) 
                          )
                        )
                        (
                          if (zero? rightVal)
                             (error 'Interpreter_getExpVal "the right of Div must not be 0")
                             (/
                                (ExpVal_getSchemeVal 
                                    (InterpreterCPS_getSimpleExpVal left env)
                                )
                                rightVal   
                             )
                            
                        )

                    )
                )
            )


            (ExprSimple_IsZero (expr)
                (let (
                      (schemeVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal expr env)))
                     )
                     (if (zero? schemeVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
            
            (ExprSimple_Greater (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  left env)))
                      (rightVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  right env)))
                     )
                     (if (> leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

            (ExprSimple_GreaterEqual (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  left env)))
                      (rightVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  right env)))
                     )
                     (if (>= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

             
            (ExprSimple_Equal (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  left env)))
                      (rightVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal  right env)))
                     )
                     (if (= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )

             
            (ExprSimple_Less (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal right env)))
                     )
                     (if (< leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
              
            (ExprSimple_LessEqual (left right)
                (let (
                      (leftVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal left env)))
                      (rightVal (ExpVal_getSchemeVal (InterpreterCPS_getSimpleExpVal right env)))
                     )
                     (if (<= leftVal rightVal)
                        (ExpVal_Bool #t)
                        (ExpVal_Bool #f)
                     )
                )
            )
            

            (ExprSimple_Cons (left right)
                (let (
                      (leftVal (InterpreterCPS_getSimpleExpVal left env))
                      (rightVal (InterpreterCPS_getSimpleExpVal right env))
                     )
                     (ExpVal_LinkList leftVal rightVal)
                )
            )

            (ExprSimple_Car (expr)
                (ExpVal_car (InterpreterCPS_getSimpleExpVal expr env)
                )
            )

            (ExprSimple_Cdr (expr)
                (ExpVal_cdr (InterpreterCPS_getSimpleExpVal expr env)
                )
            )

            (ExprSimple_EmptyList ()
                (ExpVal_EmptyListVal)
            )
            
            ;List<ExprSimple>->ExpVal_List<ExpVal>
            (ExprSimple_List (args)
                (letrec (
                          (getLinkList
                              (lambda (exprs)
                                 (if (eqv? '() exprs)
                                     (ExpVal_EmptyListVal)
                                     (ExpVal_LinkList 
                                        (InterpreterCPS_getSimpleExpVal (car exprs) env) 
                                        (getLinkList (cdr exprs))
                                     )
                                 )
                              )   
                          )
                        )
                        (getLinkList args)
                )
            )

            (ExprSimple_Lambda (vars body)
               (ExpVal_Lambda 
                  (Proc_create vars body env)
               ) 
            )
        )
    )
)


;ExpVal * list<ExprSimple>->ExpVal
(define InterpreterCPS_getProcExpVal
    (lambda (expVal exprs env)
      (cases ExpVal expVal
         (ExpVal_Lambda (proc)
            (InterpreterCPS_getTailFormExpVal 
               (Proc_getBody proc)
               (InterpreterCPS_callBind 
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

 ;Env * Env * list<symble> * list<ExprSimple> -> Env
 (define InterpreterCPS_callBind
    (lambda (env saveEnv vars exprs)
       (let (
              (vals (map (InterpreterCPS_expSimple2expVal env) exprs))
            )
            (Env_putList saveEnv vars vals)
       )
    )
)


(define InterpreterCPS_expSimple2expVal
    (lambda (env)
        (lambda (expr)
            (InterpreterCPS_getSimpleExpVal expr env)
        )
    )
)

;Env * list<symbol> * list<ExprSimple> -> Env
(define InterpreterCPS_letBind
    (lambda (env vars exprs)
        (let (
                (expVals 
                    (map
                        (lambda (expr)
                            (InterpreterCPS_getSimpleExpVal expr env)
                        )
                        exprs
                    )
                )
             )
             (begin 
                (InterpreterCPS_procEnvBuild! env vars expVals)
                (Env_putList env vars expVals)        
             )
        )
    )
)

(define InterpreterCPS_procEnvBuild!
    (lambda (env vars expVals)
     (let 
        (
          (lamdaPairList
            (InterpreterCPS_filterLambda vars expVals)
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

;list<symbol> * list<ExpVal> -> list(Pair<symobol,ExpVal_Lambda>)
(define InterpreterCPS_filterLambda
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
                           (InterpreterCPS_filterLambda tailVars tailExpVals)
                        )
                    )
                    (else
                        (InterpreterCPS_filterLambda tailVars tailExpVals)
                    )
                )
            )
        )
    )
)