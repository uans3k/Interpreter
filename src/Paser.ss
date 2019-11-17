(load "./eopl.ss")
(load "./Expression.ss")

(define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("#" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
(define the-grammar
    '((Program (Expression) Program_Default)
      (Expression (number) Expression_Const)
      (Expression (identifier) Expression_Var)
      (Expression ("true") Expression_True)
      (Expression ("false") Expression_False)
      (Expression ("-" "(" Expression "," Expression ")") Expression_Diff)
      (Expression ("+" "(" Expression "," Expression ")") Expression_Add)
      (Expression ("*" "(" Expression "," Expression ")") Expression_Mult)
      (Expression ("/" "(" Expression "," Expression ")") Expression_Div)
      (Expression ("<" "(" Expression "," Expression ")") Expression_Less)
      (Expression ("<=" "(" Expression "," Expression ")") Expression_LessEqual)
      (Expression ("=" "(" Expression "," Expression ")") Expression_Equal)
      (Expression (">" "(" Expression "," Expression ")") Expression_Greater)
      (Expression (">=" "(" Expression "," Expression ")") Expression_GreaterEqual)
      (Expression ("cons" "(" Expression "," Expression ")") Expression_Cons)
      (Expression ("car" "(" Expression "," Expression ")") Expression_Car)
      (Expression ("cdr" "(" Expression "," Expression ")") Expression_Cdr)
      (Expression ("print" "(" Expression ")") Expression_Print)
      (Expression ("printn" "(" Expression ")") Expression_Printn)
      (Expression ("emptylist") Expression_EmptyList)
      (Expression ("list" "(" (separated-list Expression ",") ")") Expression_List)
      (Expression ("cond" (arbno Expression ":" Expression) "end") Expression_Cond)
      (Expression ("default") Expression_Default)
      (Expression ("zero?" "(" Expression ")") Expression_IsZero)
      (Expression ("if" Expression "then" Expression "else" Expression) Expression_IfElse)
      (Expression ("let" (arbno identifier "=" Expression) "in" Expression) Expression_Let)
      (Expression ("(" (separated-list identifier ",") ")" "=>" "(" Expression ")" ) Expression_Lambda)
      (Expression ("[" Expression "]" "(" (separated-list Expression ",") ")") Expression_ProcCall)
      (Expression ("new" "(" Expression ")") Expression_NewRef)
      (Expression ("deref" "(" Expression ")") Expression_DeRef)
      (Expression ("set" "(" Expression "," Expression ")") Expression_SetRef)
      (Expression ("begin" (separated-list Expression ";") "end") Expression_Begin)
      )
)



(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))


