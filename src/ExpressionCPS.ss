(define-datatype ExprSimple ExprSimple?
    ;ExprSimple:=Number
    ;number->ExprSimple
    (ExprSimple_Const
        (num number?)
    )
    (ExprSimple_True)
    (ExprSimple_False)
    (ExprSimple_Default)

    ;ExprSimple := -(ExprSimple ExprSimple)
    ;ExprSimple * ExprSimple -> ExprSimple
    (ExprSimple_Diff
        (left ExprSimple?)
        (right ExprSimple?)
    )
    
    (ExprSimple_Mult
        (left ExprSimple?)
        (right ExprSimple?)
    )


    (ExprSimple_Add
        (left ExprSimple?)
        (right ExprSimple?)
    )
    (ExprSimple_Div
        (left ExprSimple?)
        (right ExprSimple?)
    )
    
    (ExprSimple_Less
        (left ExprSimple?)
        (right ExprSimple?)
    )

  

    (ExprSimple_Greater
        (left ExprSimple?)
        (right ExprSimple?)
    )
    
    (ExprSimple_GreaterEqual
        (left ExprSimple?)
        (right ExprSimple?)
    )

    (ExprSimple_LessEqual 
        (left ExprSimple?)
        (right ExprSimple?)
    )

    (ExprSimple_Equal
        (left ExprSimple?)
        (right ExprSimple?)    
    )


    (ExprSimple_Cons
        (left ExprSimple?)
        (right ExprSimple?)
    )

    (ExprSimple_Car
        (expr ExprSimple?)
    )

    (ExprSimple_Cdr
        (expr ExprSimple?)
    )

    (ExprSimple_EmptyList)


    
    (ExprSimple_List
        (exprs (list-of ExprSimple?))
    )
    
    ;ExprSimple:=zero? (ExprSimple)
    ;ExprSimple->ExprSimple
    (ExprSimple_IsZero
        (expr ExprSimple?)
    )

    
    (ExprSimple_Lambda
        (vars (list-of symbol?))
        (body ExprTailForm?)
    )

    
    ;ExprSimple:= Identifier
    (ExprSimple_Var
        (var symbol?)
    )

)

(define-datatype ExprTailForm ExprTailForm?
    (ExprTailForm_Simple 
        (expr ExprSimple?)
    )

    (ExprTailForm_Call 
        (rator ExprSimple?) 
        (rands (list-of ExprSimple?))
    )

    (ExprTailForm_Let
        (vars (list-of symbol?))
        (exprs (list-of ExprSimple?))
        (body ExprTailForm?)
    )

    (ExprTailForm_IfElse
        (predExpr ExprSimple?)
        (trueExpr ExprTailForm?)
        (falseExpr ExprTailForm?)
    )
    
    (ExprTailForm_Print
        (expr ExprSimple?)
        (body ExprTailForm?)
    )
    (ExprTailForm_Printn
        (expr ExprSimple?)
        (body ExprTailForm?)
    )

    (ExprTailForm_Cond
        (exprs (list-of ExprSimple?))
        (acts (list-of ExprTailForm?))
    )
    (ExprTailForm_NewRef
        (expr ExprSimple?)
        (cont ExprSimple?)
    )

    (ExprTailForm_DeRef
        (expr ExprSimple?)
        (cont ExprSimple?)
    )

    (ExprTailForm_SetRef
        (left ExprSimple?)
        (right ExprSimple?)
        (cont ExprTailForm?)
    )

    (ExprTailForm_Begin
        (cont ExprTailForm?)
    )
)