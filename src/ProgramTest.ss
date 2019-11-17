
(define Test_Var
    "
    x
    "
)


(define Test_Lambda
    "
    (x)=>(x)
    "
)




(define Test_LetSimple
    "
    let x = 2
    in x
    "
)

(define Test_Let
    "
    let x = 2
        y = 3
        z = 5
    in +(+(x,y),z)
    "
)

(define Test_ProcDefine
    "
    let ap = (x,y)=>
             (
               print(+(x,y))
             )       
    in 1
    "
)

(define Test_ProcCall
    "
    let ap = (x,y)=>
             (
               printn(+(x,y))
             )       
    in [ap](3,4)
    "
)

(define Test_Curry
    "
    let sum =(x)=>
             (
                (y)=>
                (
                    (z)=>
                    (
                      printn(+(+(x,y),z)) 
                    )
                )
             ) 
       
    in [[[sum](6)](4)](10)
    "
)

(define Test_New
    "
    let x = new(10)
        y = new(20)
    in 1
    "
)

(define Test_Begin
    "
    begin
       printn(1);
       printn(2);
       printn(3)
    end
    "
)

(define Test_SetRef
    "
    let x = new(10)
        y = new(20)
    in set(x,20)
    "
)
(define Test_DeRef
    "
    let x = new(10)
        y = new(20)
    in deref(x)
    "
)

(define Test_BeginAndRef
    "
    let x = new(10)
        y = new(20)
    in begin
        set(x,20);
        printn(deref(x));
        set(y,30);
        printn(deref(y))
       end
    "
)

(define Test_IfElse
    "
    if =(1,0)
    then printn(1111111111111111)
    else printn(2222222222222222)
    "
)


(define Test_Recursion
    "
    let rec = (x)=>
               (if =(0,x)
                then printn(1111111111111111)
                else begin
                        printn(x);
                        [rec](-(x,1))
                     end
               )
    in  [rec](10)
    "
)

(define Test_MultiRecursion
    "
    let even = (x)=>
               (if =(0,x)
                then printn(22222222222222222)
                else begin
                        printn(2);
                        [odd](-(x,1))
                     end
               )

        odd = (x)=>
              (if =(0,x)
               then printn(1111111111111111)
               else begin
                        printn(1);
                        [even](-(x,1))
                    end
              )
    in  [even](10)
    "
)


(define Test_TryCatch
    "
    try begin
          printn(1);
          try
            raise 2
          catch 2
            begin
                printn(22222222222222);
                try
                   raise 1
                catch 3
                   printn(333333333333)
            end    
        end
    catch 1
       printn(11111111111111111)
    "
)




; (define Test_Thread
;         "
;         let 
;             n1=10
;         in 
;             let 
;                 th1= ()=>
;                      (let count=(x)=>
;                                 (if =(x,0)
;                                  then printn(222222222222222222)
;                                  else   
;                                      begin
;                                         printn(x);
;                                         [count](-(x,1))
;                                      end
;                                 )
;                       in [count](n1)
;                      )
;             in
;                 begin
;                     thread(th1);
;                     printn(111111111111111111111111111111)
;                 end
;         "
; )


(define Test_Thread
    "
    let 
        n1=10
        n2=20
        count=(x)=>
        (
            if =(x,0)
            then printn(111111111111111111)
            else   
               begin
                   printn(x);
                   [count](-(x,1))
               end
        )            
    in 
        let 
            th1= ()=>
                 (
                    [count](n1)
                 )
            th2= ()=>
                 (
                    [count](n2)
                 )
        in
            begin
                thread(th1);
                thread(th2);
                printn(3333333333333333)
            end
    "
)

(define Test_PV
    "
    let m=semaphore(1) 
    in
       begin
           p(m);
           v(m)
       end
    "
)


(define Test_Semaphore
    "
    let 
        n1=20
        n2=40
        m=semaphore(1) 
    in 
        let 
            th1= ()=>
                 (let
                    count=(x)=>
                    (
                        if =(x,0)
                        then 
                            begin
                                printn(111111111111111111);
                                v(m)
                            end
                        else   
                           begin
                               printn(x);
                               [count](-(x,1))
                           end
                    )
                  in
                      begin
                          p(m);
                          [count](n1)              
                      end             
                 )
            th2= ()=>
                 (let
                    count=(x)=>
                    (
                        if =(x,0)
                        then 
                            begin
                                printn(222222222222222);
                                v(m)
                            end
                        else   
                           begin
                               printn(x);
                               [count](-(x,1))
                           end
                    )
                  in
                      begin
                          p(m);
                          [count](n2)
                      end             
                 )
        in
            begin
                thread(th1);
                thread(th2);
                printn(3333333333333333)
            end
    "
)