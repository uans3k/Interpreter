(define ListTools_filter
    (lambda (func in)
        (letrec (
                  (filterInner 
                    (lambda (inList)
                         (if (eqv? inList '())
                            inList
                            (let (
                                   (head (car inList))
                                   (tail (cdr inList))
                                 )
                                 (if (func head)
                                    (cons head (filterInner tail))
                                 )
                                 (filterInner tail)
                            )
                         )
                    )
                  )
                )
                (filterInner in)
        )
    )
)

;list<T> -> Maybe<T>
(define ListTools_tail
    (lambda (lst)
        (letrec 
            (
                (findTail 
                    (lambda (l)
                      (if (null? (cdr l))
                          (car l)
                          (findTail (cdr l))
                      )
                    )
                )
            )
            (if (null? lst)
                (#f)
                (findTail lst)
            )
        )
    )
)


;list<T>* int * (T->boolean) -> Maybe<int>
(define ListTools_index
  (lambda (lst sIndex pred)
     (letrec 
          (
              (indexInner
                (lambda (l s p)
                  (cond
                      ((< s 0)
                        (error 'CPS_listIndex "can not index from (sIndex<0)")
                      )
                      ((null? l)
                          #f
                      ) 
                      ((> s 0)
                          (indexInner (cdr l) (- s 1) p)
                      )
                      (else
                          (cond
                              ((pred (car l))
                                  sIndex
                              )
                              ((indexInner (cdr l) s p) => 
                                  (lambda (n) (+ n 1))
                              )
                              (else
                                  #f
                              )
                          )  
                      )
                  )
                )
              )
          )
          (indexInner lst sIndex pred)
     )
  )
)

(define ListTools_set
    (lambda (lst n val)
        (cond
            ((null? lst) 
                (error 'ListTools_set "out of bound")
            )
            ((zero? n) 
                (cons val (cdr lst))
            )
            (else 
              (cons (car lst) 
                  (ListTools_set (cdr lst) (- n 1) val)
              )
            )
        )
    )
)

