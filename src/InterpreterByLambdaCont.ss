(load "./eopl.ss")
(load "./Expression.ss")
(load "./Env.ss")
(load "./ExpVal.ss")
(load "./Sto.ss")

;Expression * Env -> ExpVal
(define InterpreterByLambdaCont_getExpVal
    (lambda (expression env cont)
        (cases Expression expression

            (Expression_Const (num)
                (Cont_continue cont
                    (ExpVal_Number num)
                )
            )

            (Expression_Default ()
                (Cont_continue cont
                    (ExpVal_Bool #t)
                )
            )

            (Expression_True  ()
                (Cont_continue cont
                    (ExpVal_Bool #t)
                )
            )

             (Expression_False ()
                (Cont_continue cont
                    (ExpVal_Bool #f)
                )
            )

            (Expression_Diff (left right)
               (InterpreterByLambdaCont_getExpVal
                 left
                 env
                 (Cont_DiffRight right env cont)
               )
            )
            
            (Expression_Add (left right)
               (InterpreterByLambdaCont_getExpVal
                 left
                 env
                 (Cont_AddRight right env cont)
               )
            )

            (Expression_Mult (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_MultRight right env cont)
               )
            )
            
            (Expression_Div (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_MultRight right env cont)
               )
            )
            

            (Expression_IsZero (expr)
               (InterpreterByLambdaCont_getExpVal
                  expr
                  env
                  (Cont_IsZero cont)
               )
            )
            
            (Expression_Greater (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_GreaterRight right env cont)
               )
            )

            (Expression_GreaterEqual (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_GreaterEqualRight right env cont)
               )
            )

             
            (Expression_Equal (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_EqualRight right env cont)
               )
            )

             
            (Expression_Less (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_LessRight right env cont)
               )
            )
              
            (Expression_LessEqual (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_LessEqualRight right env cont)
               )
            )
            

            (Expression_Cons (left right)
               (InterpreterByLambdaCont_getExpVal
                  left
                  env
                  (Cont_ConsRight right env cont)
               )
            )

            (Expression_Car (expr)
                (InterpreterByLambdaCont_getExpVal expr env
                    (Cont_Car cont)
                )
            )

            (Expression_Cdr (expr)
                (InterpreterByLambdaCont_getExpVal expr env
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
                (InterpreterByLambdaCont_getExpVal
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
                (InterpreterByLambdaCont_getExpVal 
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
                (InterpreterByLambdaCont_getExpVal
                    predExp
                    env
                    (Cont_IfElse trueExp falseExp env cont)
                )
            )
            
            (Expression_Print (expr)
                (InterpreterByLambdaCont_getExpVal
                    expr
                    env
                    (Cont_Print cont)
                )
            )
            
            (Expression_Printn (expr)
                (InterpreterByLambdaCont_getExpVal
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
               (InterpreterByLambdaCont_getExpVal
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
                (InterpreterByLambdaCont_getExpVal
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
                (InterpreterByLambdaCont_getExpVal 
                    expr 
                    env
                    (Cont_NewRef cont)
                )
            )
            
            (Expression_DeRef (expr)
                (InterpreterByLambdaCont_getExpVal 
                    expr 
                    env
                    (Cont_DeRef cont)
                )
            )
            
            (Expression_SetRef (left right)
                (InterpreterByLambdaCont_getExpVal 
                    left 
                    env
                    (Cont_SetRefRight right env cont)
                )
            )

            (Expression_Begin (exprs)
                (InterpreterByLambdaCont_getExpVal
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
                (error 'InterpreterByLambdaCont_getExpVal "Expression type error")
            )
        )
    )
)



(define InterpreterByLambdaCont_getProcExpVal
    (lambda (expValProc expVals cont)
      (cases ExpVal expValProc
         (ExpVal_Lambda (proc)
            (InterpreterByLambdaCont_getExpVal 
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
           (error 'InterpreterByLambdaCont_getExpVal "it is not a proc")
         )
      )
    )
)



;Env * list<symbol> * list<Expression> -> Env
(define InterpreterByLambdaCont_letBind
    (lambda (env vars expVals)
        (begin 
            (InterpreterByLambdaCont_procEnvBuild! env vars expVals)
            (Env_putList env vars expVals)        
        )   
    )
)

(define InterpreterByLambdaCont_procEnvBuild!
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
(define InterpreterByLambdaCont_getProgramExpVal
    (lambda (program)
        (cases Program program
            (Program_Default (expr)
                (Sto_init)
                (InterpreterByLambdaCont_getExpVal expr (Env_createDefault) (Cont_createEnd))
            )
            (else 
                (error 'InterpreterByLambdaCont_getProgramExpVal "Program type error")
            )
        )
    )
)

;Program -> SchemeValue
(define InterpreterByLambdaCont_runProgram
    (lambda (program)
        (ExpVal_getSchemeVal
            (InterpreterByLambdaCont_getProgramExpVal program)
        )
    )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cont ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;()->Cont
(define Cont_endCont
    (lambda ()
        (lambda (val)
            (begin
                (display "end cont!!")
                val
            )  
        )
    )
)

;Cont->Cont
(define Cont_Print
    (lambda (cont)
        (lambda (val)
            (begin
                (display 
                    (ExpVal_getSchemeVal val)
                )
                (cont 1)
            )  
        )
    )
)

(define Cont_Printn
    (lambda (cont)
        (lambda (val)
            (begin
                (display 
                    (ExpVal_getSchemeVal val)
                )
                (display #\newline)
                (cont 1)
            )  
        )
    )
)

(define Cont_IsZero
    (lambda (cont)
        (lambda (val)
            (cont
                (ExpVal_Bool
                    (zero?
                        (ExpVal_getSchemeVal val)
                    )
                )
            )
        )
    )
)

;Expression * Env * Cont -> Cont
(define Cont_ConsRight
    (lambda (right env cont)
        (lambda (val)
            (InterpreterByLambdaCont
                right
                env
                (Cont_ConsEnd val cont)
            )
        )
    )
)

(define Cont_ConsEnd
    (lambda (expVal cont)
        (lambda (val)
            (cont
                (ExpVal_LinkList expVal val)
            )
        )
    )
)

(define Cont_Car
    (lambda (cont)
        (lambda (val)
            (cases ExpVal val
                (ExpVal_LinkList (left right)
                    (cont left)
                )
                (else
                    (error 'Cont_Car "can not car a non-list")
                )
            )
        )
    )
)

(define Cont_Cdr
    (lambda (cont)
        (lambda (val)
            (cases ExpVal val
                (ExpVal_LinkList (left right)
                    (cont right)
                )
                (else
                    (error 'Cont_Car "can not car a non-list")
                )
            )
        )
    )
)

(define Cont_List
    (lambda (expVals exprs env cont)
        (lambda (val)
            (if (eqv? exprs '())
                (cont
                    (ExpVal_buildLinkList
                        (append expVals (list val))
                    )    
                )
                (InterpreterByLambdaCont_getExpVal
                    (car exprs)
                    env
                    (Cont_List 
                        (append expVals (list val))
                        (cdr exprs)
                        env
                        cont
                    )
                )            
            )
        )
    )
)

(define Cont_Cond
    (lambda (conds acts env cont)
        (lambda (val)
            (if (ExpVal_getSchemeVal val)
                (InterpreterByLambdaCont_getExpVal
                    (car acts)
                    env
                    cont
                )
                (if (eqv? conds '())
                    (error 'Cont_Cond "there is not a default act")
                    (InterpreterByLambdaCont_getExpVal
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
    )
)

(define Cont_Let
    (lambda (vars expVals exprs body env cont)
        (lambda (expVal)
            (if (eqv? exprs '())
                (InterpreterByLambdaCont_getExpVal
                    body
                    (InterpreterByLambdaCont_letBind env vars 
                        (append expVals (list expVal))
                    )
                    cont
                )
                (InterpreterByLambdaCont_getExpVal
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
    )
)

(define Cont_ProCallBindValInit
    (lambda (exprs env cont)
        (lambda (expVal)
            (if (eqv? exprs '())
                (InterpreterByLambdaCont_getProcExpVal
                    expVal
                    '()
                    cont
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
                )
            )            
        )
    )
)

(define Cont_ProcCallBindVal
    (lambda (proc exprs expVals env cont)
        (lambda (expVal)
            (if (eqv? exprs '())
                (InterpreterByLambdaCont_getProcExpVal
                    proc
                    (append expVals (list expVal))
                    cont    
                )
                (InterpreterByLambdaCont_getExpVal
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
    )
)

(define Cont_NewRef
    (lambda (cont)
        (lambda (expVal)
            (cont
                (ExpVal_Ref
                    (Sto_newRef! expVal)
                )                 
            )
        )
    )
)

(define Cont_DeRef 
    (lambda (cont)
        (lambda (expVal)
            (ExpVal_Ref (ref)
                (cont
                    (Sto_deRef ref)
                )            
            )
            (else
                (error 'Cont_DeRef "can't deref a non-ref")
            )
        )
    )
)

(define Cont_SetRefRight
    (lambda (right env cont)
        (lambda (expVal)
             (cases ExpVal expVal
                (ExpVal_Ref (ref)
                    (InterpreterByCont_getExpVal
                        right
                        env
                        (Cont_SetRefEnd 
                            ref
                            cont
                        )    
                    )
                )
                (else
                    (error 'InterpreterByCont_getExpVal "can't set! a non-ref")
                )    
            )    
        )
    )
)

(define Cont_SetRefEnd
    (lambda  (ref cont)
        (lambda (expVal)
            (cont
                (begin 
                    (Sto_setRef! ref expVal)
                    (ExpVal_Number 1)
                )        
            )
        )
    )
)

(define Cont_Begin
    (lambda (exprs env cont)
        (lambda (expVal)
            (if (eqv? exprs '())
                (Cont_continue cont
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
                )
            )
        )
    )
)

