(load "./Expression.ss")
(load "./ExpressionCPS.ss")
(load "./ListTools.ss")

(define CPSLambda_IDPREFIX "var_")
(define CPSLambda_CONTVAR '__cont__)


;Program->ExprTailForm
(define CPSLambda_program2CPSLambdaProgram
    (lambda (program)
        (cases Program program
            (Program_Default (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (ExprTailForm_Simple 
                            (car simples)
                        )
                    )
                )
            )
            (else 
                (error 'CPSLambda_program2CPSLambdaProgram "it's a not program")
            )
        )
    )
)


;list<Expression> * (List<ExprSymple>->ExprTailForm)-> ExprTailForm
(define CPSLambda_exprList2ExprTailForm
    (lambda (exprs builder)
        (let convertRest 
             (
                (workExprs exprs)
                (sIndex 0)
             )
             (let
                (
                    (pos
                        (ListTools_index workExprs sIndex
                            (lambda (expr)
                                (not 
                                    (CPSLambda_isSimple expr)
                                )
                            )
                        ) 
                    )
                )
                (if (not pos)
                    (builder 
                        (map CPSLambda_expr2ExprSimple workExprs)
                    )
                    (let
                        (
                            (var (CPSLambda_getNewVar))
                        )
                        (CPSLambda_expr2ExprTailForm
                            (list-ref workExprs pos)
                            (ExprSimple_Lambda
                                (list var)
                                (convertRest
                                    (ListTools_set workExprs pos var)
                                    (+ pos 1)
                                )
                            )
                        )
                    )
                )    
             )
        )
    )    
)


;Expression * ExprSimple -> ExprTailFrom
;ExprSimple === ExprSimple_Lambda ====cont
(define CPSLambda_expr2ExprTailForm
    (lambda (inExpr cont)
        (cases Expression inExpr

            (Expression_Diff (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Diff left right cont)
            )

            (Expression_Mult (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Mult left right cont)
            )

            (Expression_Add (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Add left right cont)
            )

            (Expression_Div (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Div left right cont)
            )

            (Expression_Less (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Less left right cont)
            )

            (Expression_LessEqual (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_LessEqual left right cont)
            )

            (Expression_Greater (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Greater left right cont)
            )

            (Expression_GreaterEqual (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_GreaterEqual left right cont)
            )
            
            (Expression_Equal (left right)
                (CPSLambda_binaryExpr2ExprTailForm ExprSimple_Equal left right cont)
            )

            (Expression_Car (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (CPSLambda_makeSingleArgCall
                            cont
                            (ExprSimple_Car
                                (car simples)
                            )
                        )
                    )
                )
            )

            (Expression_Cdr (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (CPSLambda_makeSingleArgCall
                            cont
                            (ExprSimple_Cdr
                                (car simples)
                            )
                        )
                    )
                )
            )

            (Expression_List (exprs)
                (CPSLambda_exprList2ExprTailForm
                    exprs
                    (lambda (simples)
                        (CPSLambda_makeSingleArgCall
                            cont
                            (ExprSimple_List simples)
                        )
                    )
                )
            )
            
            (Expression_IsZero (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list exprs)
                    (lambda (simples)
                        (CPSLambda_makeSingleArgCall
                            cont
                            (ExprSimple_IsZero
                                (car simples) 
                            )
                        )
                    )
                )
            )
            
            

            (Expression_Print (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (ExprTailFrom_Print
                            (car simples)
                            (CPSLambda_makeSingleArgCall
                                cont
                                (ExprSimple_Const 1)
                            )
                        )
                    )
                )
            )

            (Expression_Printn (expr)
               (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (ExprTailFrom_Printn
                            (car simples)
                            (CPSLambda_makeSingleArgCall
                                cont
                                (ExprSimple_Const 1)
                            )
                        )
                    )
               )
            )

            (Expression_ProcCall (rator rands)
                (CPSLambda_exprList2ExprTailForm 
                    (cons rator rands)
                    (lambda (simples)
                        (ExprTailForm_Call
                            (car simples)
                            (append (cdr simples) (list cont))
                        )
                    )
                )
            )

            (Expression_Let (vars exprs body)
                (CPSLambda_exprList2ExprTailForm
                    exprs
                    (lambda (simples)
                        (ExprTailForm_Let vars simples
                            (CPSLambda_expr2ExprTailForm body cont)
                        )
                    )
                )
            )

            (Expression_IfElse (predExpr trueExpr falseExpr)
                (CPSLambda_exprList2ExprTailForm
                    (list predExpr)
                    (lambda (simples)
                        (let
                            (
                                (var (CPSLambda_getNewVar))
                            )
                            (CPSLambda_makeSingleArgCall
                                (ExprSimple_Lambda 
                                    (list var)
                                    (ExprTailForm_IfElse
                                        (car simples)
                                        (CPSLambda_expr2ExprTailForm trueExpr var)
                                        (CPSLambda_expr2ExprTailForm falseExpr var)
                                    )
                                )
                                cont
                            )
                        )
                    )
                )
            )

            
            (Expression_Cond (conds acts)
                (CPSLambda_exprList2ExprTailForm
                    (list conds)
                    (lambda (simples)
                        (let
                            (
                                (var (CPSLambda_getNewVar))
                            )
                            (CPSLambda_makeSingleArgCall
                                (ExprSimple_Lambda 
                                    (list var)
                                    (ExprTailForm_Cond
                                        simples
                                        (map 
                                            (lambda (expr)
                                                (CPSLambda_expr2ExprTailForm expr var)
                                            )
                                            acts
                                        )
                                    )
                                )
                                cont
                            )
                        )
                    ) 
                )
            )

            (Expression_NewRef (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (ExprTailForm_NewRef
                            (car simples)
                            cont
                        )
                    )
                )
            )
            (Expression_DeRef (expr)
                (CPSLambda_exprList2ExprTailForm
                    (list expr)
                    (lambda (simples)
                        (ExprTailForm_DeRef
                            (car simples)
                            cont
                        )
                    )
                )
            )

            (Expression_SetRef (left right)
                (CPSLambda_exprList2ExprTailForm
                    (list left right)
                    (lambda (simples)
                        (ExprTailForm_SetRef
                            (car simples)
                            (cadr simples)
                            (CPSLambda_makeSingleArgCall
                                cont
                                (ExprSimple_Const 1)
                            )
                        )
                    )
                )
            )

            (Expression_Begin (exprs)
                (CPSLambda_exprList2ExprTailForm
                    exprs
                    (lambda (simples)
                        (CPSLambda_makeSingleArgCall
                            cont
                            (ListTools_last simples)
                        )
                    )
                )
            )
        )
    )
)



;ExprSimple_Lambda|ExprSimple_Var * ExprSimple -> ExprTailForm_Call
;ExprSimple*ExprSimple->ExprTailForm
(define CPSLambda_makeSingleArgCall
    (lambda (cont expr)
        (ExprTailFrom_Call cont
            (list expr)
        )
    )
)



;Expression -> ExprSimple
(define CPSLambda_expr2ExprSimple
    (lambda (expr)
        (cases Expression expr
            (Expression_Const (num)
                (ExprSimple_Const num)
            )

            (Expression_True ()
                (ExprSimple_Ture)
            )
            
            (Expression_False ()
                (ExprSimple_False)
            )
        
            (Expression_Var (var)
                (ExprSimple_Var var)
            )

            (Expression_EmptyList ()
                (ExprSimple_EmptyList)
            )

            (Expression_Default ()
                (ExprSimple_Default)
            )


            ;append CONT_VAR because wo will bind cont when we call 
            (Expression_Lambda (vars body)
                (ExprSimple_Lambda
                    (append vars (list CPSLambda_CONTVAR))
                    (CPSLambda_expr2ExprTailForm body CPSLambda_CONTVAR)
                )    
            )

            (Expression_Diff (left right)
                (ExprSimple_Diff
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
    
            (Expression_Mult  (left right)
                (ExprSimple_Mult
                    (CPSLambda_expr2ExprSimple left right)
                )
            )


            (Expression_Add (left right)
                (ExprSimple_Add
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
            (Expression_Div (left right)
                (ExprSimple_Div
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
    
            (Expression_Less (left right)
                (ExprSimple_Less
                    (CPSLambda_expr2ExprSimple left right)
                )
            )

            (Expression_LessEqual (left right)
                (ExprSimple_LessEqual
                    (CPSLambda_expr2ExprSimple left right)
                )
            )

            (Expression_Greater (left right)
                (ExprSimple_Greater
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
            
            (Expression_GreaterEqual (left right)
                (ExprSimple_GreaterEqual
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
        
        
        
            (Expression_Equal (left right)
                (ExprSimple_Equal
                    (CPSLambda_expr2ExprSimple left right)
                )   
            )
        
            (Expression_IsZero (expr)
                (ExprSimple_IsZero
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
        
            (Expression_Cons (left right)
                (ExprSimple_Cons
                    (CPSLambda_expr2ExprSimple left right)
                )
            )
            
            (Expression_Car (expr)
                (ExprSimple_Car
                    (CPSLambda_expr2ExprSimple expr)
                )
            )

            (Expression_Cdr (expr)
                (ExprSimple_Cdr
                    (CPSLambda_expr2ExprSimple expr)
                )
            )

            
            (Expression_List (exprs)
                (ExprSimple_List
                    (map CPSLambda_expr2ExprSimple exprs)
                )
            )
   
            
            (else
                (error 'CPSLambda_expr2ExprSimple "it is not a ExprSimple")
            )
        )
    )
)



;Expression -> boolean

(define CPSLambda_isSimple
    (lambda (expr)
        (cases Expression expr

            (Expression_Const (num)
              #t
            )

            (Expression_True ()
              #t
            )
            
            (Expression_False ()
              #t
            )

            (Expression_EmptyList ()
                #t
            )

            (Expression_Default ()
                #t
            )

            (Expression_Var (var)
                #t
            )

            (Expression_Lambda (vars body)
                #t
            )
            
            (Expression_Diff (left right)
              (and
                (CPSLambda_isSimple left)
                (CPSLambda_isSimple right)
              )
            )
    

            (Expression_Mult  (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )


            (Expression_Add (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )

            (Expression_Div (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )
    
            (Expression_Less (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )

            (Expression_LessEqual (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )

            (Expression_Greater (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )
            
            (Expression_GreaterEqual (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )
        
        
        
            (Expression_Equal (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )   
            )
        
            (Expression_IsZero (expr)
                (CPSLambda_isSimple expr)
            )
        
            (Expression_Cons (left right)
                (and
                    (CPSLambda_isSimple left)
                    (CPSLambda_isSimple right)
                )
            )
            
            (Expression_Car (expr)
                (CPSLambda_isSimple expr)
            )

            (Expression_Cdr (expr)
                (CPSLambda_isSimple expr)
            )

            (Expression_List (exprs)
                (CPSLambda_isAllSimple exprs)
            )
            
            (else #f)
        )
    )            
)


(define CPSLambda_binaryExpr2ExprTailForm
    (lambda (op left right cont)
        (CPSLambda_exprList2ExprTailForm
            (list left right)
            (lambda (simples)
                (CPSLambda_makeSingleArgCall
                    cont
                    (op
                        (car simples)
                        (cadr simples)
                    )
                )
            )
        )
    )
)

;Expression -> boolean
(define CPSLambda_isAllSimple
    (lambda (exps)
        (if (null? exps)
            #t
            (and 
                (CPSLambda_isSimple? (car exps))
                (CPSLambda_isAllSimple? (cdr exps))
            )
        )
    )
)

;() => symble
(define CPSLambda_getNewVar
    (let 
        ((i 0))
        (lambda ()
           (begin
               (set! i (+ i 1))
               (string->symbol
                   (string-append
                       CPSLambda_IDPREFIX        
                       (number->string i)
                   )
               )
           )
        )
    )
)

