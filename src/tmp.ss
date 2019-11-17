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

    (Cont_ProCallBindVal
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
        (right Expression？)
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
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_MultEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_AddRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_AddEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_DivRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_DivEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_LessRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_LessEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_LessEqualRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_LessEqualEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_GreaterRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_GreaterEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_GreaterEqualRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_GreaterEqualEnd
        (left ExpVal?)
        (cont Cont?)
    )
    (Cont_EqualRight
        (right Expression?)
        (savaEnv Env?)
        (cont Cont?)
    )
    (Cont_EqualEnd
        (left ExpVal?)
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
        (env Env)
        (cont Cont?)
    )

    (Cont_IfElse
        (trueExp Expression?)
        (falseExp Expression?)
        (env Env?)
        (cont Cont?)
    )
)