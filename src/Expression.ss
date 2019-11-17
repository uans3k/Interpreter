(load "./eopl.ss")

(define-datatype Expression Expression?
    ;Expression:=Number
    ;number->Expression
    (Expression_Const
        (num number?)
    )
    (Expression_True)
    (Expression_False)

    ;Expression := -(Expression Expression)
    ;Expression * Expression -> Expression
    (Expression_Diff
        (left Expression?)
        (right Expression?)
    )
    
    (Expression_Print
        (expr Expression?)
    )

    (Expression_Printn
        (expr Expression?)
    )

    (Expression_Mult
        (left Expression?)
        (right Expression?)
    )


    (Expression_Add
        (left Expression?)
        (right Expression?)
    )
    (Expression_Div
        (left Expression?)
        (right Expression?)
    )
    
    (Expression_Less
        (left Expression?)
        (right Expression?)
    )

    (Expression_Cons
        (left Expression?)
        (right Expression?)
    )

    (Expression_Car
        (expr Expression?)
    )

    (Expression_Cdr
        (expr Expression?)
    )

    (Expression_EmptyList)

    (Expression_Default)

    (Expression_List
        (exprs 
            (list-of Expression?)
        )
    )



    (Expression_Greater
        (left Expression?)
        (right Expression?)
    )
    
    (Expression_GreaterEqual
        (left Expression?)
        (right Expression?)
    )

    (Expression_LessEqual 
        (left Expression?)
        (right Expression?)
    )

    (Expression_Equal
        (left Expression?)
        (right Expression?)    
    )

    ;Expression:=zero? (Expression)
    ;Expression->Expression
    (Expression_IsZero
        (expr Expression?)
    )

    
    (Expression_Lambda
        (vars (list-of symbol?))
        (body Expression?)
    )

    
    ;Expression:= Identifier
    (Expression_Var
        (var symbol?)
    )



    (Expression_Cond
        (conds (list-of Expression?))
        (acts  (list-of Expression?))
    )


    ;Expression:= if Expression then Expression else Expression
    (Expression_IfElse
        (predExp Expression?)
        (trueExp Expression?)
        (falseExp Expression?)
    )
    
   
    ;Expression:=let Identifier = Expression in Expression
    (Expression_Let
        (vars (list-of symbol?))
        (vals (list-of Expression?))
        (body Expression?)
    )


 



    (Expression_ProcCall
        (var Expression?)
        (vals (list-of Expression?))
    )

    (Expression_NewRef
        (expr Expression?)
    )

    (Expression_DeRef
        (expr Expression?)
    )

    (Expression_SetRef
        (left Expression?)
        (right Expression?)
    )

    (Expression_Begin
        (exprs (list-of Expression?))
    )

    (Expression_TryCatch
        (expr Expression?)
        (num number?)
        (handler Expression?)
    )

    (Expression_Raise
        (num number?)
    )

    (Expression_Thread
        (expr Expression?)
    )

    (Expression_Semaphore
        (expr Expression?)
    )

    (Expression_P
        (expr Expression?)
    )

    (Expression_V
        (expr Expression?)
    )
)

