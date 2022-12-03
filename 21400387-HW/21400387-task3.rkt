#lang plai
;type def
;implement * operator,if exp,or operator,equal operator
(define-type RCFAEDS
    [num    (n number?)]
    [add     (lhs RCFAEDS?) (rhs RCFAEDS?)]
    [sub     (lhs RCFAEDS?) (rhs RCFAEDS?)]
    ; * operator
    [mul (lhs RCFAEDS?) (rhs RCFAEDS?)] 
    [id         (name symbol?)]
    [fun      (param symbol?) (body RCFAEDS?)]
    [app     (ftn RCFAEDS?) (arg RCFAEDS?)]
    ;if exp
    [ifexp (test-expr RCFAEDS?) (then-expr RCFAEDS?)(else-expr RCFAEDS?)]
    ;or operator
    [orop (lhs RCFAEDS?) (rhs RCFAEDS?)]
    ;equal operator
    [eqop (lhs RCFAEDS?) (rhs RCFAEDS?)]) 

; strict , num-op
(define (num-op op x y)
    (numV (op (numV-n (strict x))
                        (numV-n (strict y)))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))
(define (num* x y) (num-op * x y))

; strict: LFAE-Value -> LFAE-Value
(define (strict v)
    (type-case RCFAEDS-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))    ;; box contains #f? Then evaluate expr as needed.
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))      ;; return v after evaluating it.
                          (unbox v-box))] ;; just unbox to return the value that was already evaluated once.
        [else v]))  ;; for numV or closureV


;[contract]parse: sexp -> RCFAEDS
;[purpose] to convert sexp to RCFAEDS
;add that if else condition => to solve the error of the syntax 
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        ;[(list 'with (list i d v) e)  (app (fun i (parse d) (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)     (fun p (parse b))]  ;; e.g., {fun {x} {+ x 1}}
        ;[(list 'fun (list p) b)     (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [(list 'if c t f)           (ifexp (parse c) (parse t) (parse f))]
        [(list '* l r)              (mul (parse l) (parse r))]
        [(list 'or l r)             (orop (parse l) (parse r))]
        [(list '= l r)              (eqop (parse l) (parse r))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

;[test parser]
;(parse '{with {fac {fun {n}
;                            {if {= n 0}
;                                   1
;                             {* n {fac {- n 1}}}}}}
;      {fac 10}})
;(parse '{with {mk-rec {fun {body-proc}
;                                 {with {fX {fun {fY}
;                                                         {with {f {fun {x}
;                                                                       {{fY fY} x}}}
;                                                                       {body-proc f}}}}
;                                           {fX fX}}}}
;           {with {fac {mk-rec
;                                           {fun {fac}
;                                                   ; Exactly like original fac
;                                                   {fun {n}
;                                                                 {if {= n 0}
;                                                                        1
;                                                                         {* n {fac {- n 1}}}}}}}}
;                                    {fac 10}}})


;[contract]is-recursion : sexp -> boolean
;[purpose]: to confirm sexp is recursion or not
(define (is-recursion sexp)
  (match sexp
    [(list 'with (list i a) (list f b)) (if (check-id i a) true false)]  
    [else                             false ]))

;[contract]check-id :function identifier(i) , function body(a) -> boolean
;[purpose]: check pattern of function in function body
(define (check-id i a) 
 (match a
   [(list 'fun (list p) b) (find-id i b)] ;check redundancy of id(func name) in the function body
   [else  false]
   )
  )
;[contract]find-id : i(an identifier of is-recursion/check-id function), b is a function body from check-id -> boolean
;[purpose]: find the same id in function body
(define (find-id i b)
    (match b
        [(? number?)                false]
        [(list '+ l r)                     (if(or (find-id i l) (find-id i r)) true false)]
        [(list '- l r)                      (if(or (find-id i l) (find-id i r)) true false)]
        [(list 'with (list i v) e)    (if(or (find-id i v) (find-id i e)) true false)]
        [(? symbol?)                (if(equal? i b) true false)]
        [(list 'fun (list p) body)     (if(or (find-id i p) (find-id i body)) true false)]  ;; e.g., {fun {x} {+ x 1}}
        [(list f a)                       (if(or (find-id i f) (find-id i a)) true false)]
        [(list 'if c t f)          (if(or (find-id i t) (find-id i f)) true false)] 
        [(list '* l r)             (if(or (find-id i l) (find-id i r)) true false)]
        [(list 'or (list l r))     (if(or (find-id i l) (find-id i r)) true false)]
        [(list '= l r)             (if(or (find-id i l) (find-id i r)) true false)]
        [else                             false]))

;;[test1]is-recursion                           
;(is-recursion '{with {fac {fun {n}
;                            {if {= n 0}
;                                   1
;                             {* n {fac {- n 1}}}}}}
;      {fac 10}})
;;[test2]is-recursion  
;(is-recursion '{with {fib {fun {n}
;                          {if {or {= n 0} {= n 1}}
;                              1
;                              {+ {fib {- n 1}}
;                             {fib {- n 2}}}}}}
;      {fib 10}})
;;******
;;[test1]check-id 
;(check-id 'fib '{fun {n} 
;                            {if {= n 0}
;                                   1
;                             {* n {fac {- n 1}}}}}
      
;      )


;[contract] desugar : sexp(list) -> list
;[purpose]: to make sugar sexp ->  desugar sexp
(define (desugar sexp)
  (match sexp
    [(list 'with (list i (list 'fun (list n) e)) (list i num))   (list 'with (list 'mk-rec (list 'fun (list 'body-proc)
                                                                                                 (list 'with (list 'fX (list 'fun (list 'fY)
                                                                                                                           (list 'with (list 'f (list 'fun (list 'x)
                                                                                                                                                      (list '(fY fY) 'x)))
                                                                                                                                                       '(body-proc f))))
                                                                                                                           '(fX fX)))) (list 'with (list i (list 'mk-rec (list 'fun (list i) (list 'fun (list n) e)))) (list i num))) ]))
                                                                       
                                             

;;[test1] desugar func
;(parse (desugar '{with {fac {fun {n}
;                            {if {= n 0}
;                                   1
;                             {* n {fac {- n 1}}}}}}
;      {fac 10}}))
;;[test2]
;(desugar '{with {fib {fun {n}
;                          {if {or {= n 0} {= n 1}}
;                              1
;                              {+ {fib {- n 1}}
;                             {fib {- n 2}}}}}}
;      {fib 10}})


(define-type RCFAEDS-Value
        [numV         (n number?)]
        [closureV    (param symbol?)
                             (body RCFAEDS?)
                             (ds DefrdSub?)]
        [exprV          (expr RCFAEDS?)
                              (ds DefrdSub?)
                              (value (box/c (or/c false RCFAEDS-Value?)))]
                              )
(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value RCFAEDS-Value?) (ds DefrdSub?)]
    [aRecSub  (name symbol?)
                       (value-box (box/c RCFAEDS-Value?))
                       (ds DefrdSub?)])

(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub     ()             (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                  (strict v)
                                                   (lookup name saved))]
          [aRecSub (sub-name val-box rest-ds)
                          (if (symbol=? sub-name name)
                               (unbox val-box)
                               (lookup name rest-ds))]))

;interpreter
;[contract]interp: RLFAE DefrdSub -> RLFAE-Value 
(define (interp rcfaeds ds)
    (type-case RCFAEDS rcfaeds
        [num (n)          (numV n)]
        [add  (l r)         (num+ (interp l ds) (interp r ds))]
        [sub  (l r)         (num- (interp l ds) (interp r ds))]
        [mul (l r)         (num* (interp l ds) (interp r ds))]
        [id  (name)  (lookup name ds)]
        [fun (param body-expr)  (closureV param body-expr ds)]
        [orop (l r) (or (interp l ds) (interp r ds))]
        [eqop (l r) (equal? (interp l ds) (interp r ds))]
        [app   (f a)        (local [(define f-val (strict (interp f ds)))     ;ftn-v --> f-val , + strict()                     
                                                    (define a-val (exprV a ds (box #f)))]  ; avg-v --> a-val              
                                             (interp (closureV-body f-val)
                                                          (aSub (closureV-param f-val)
                                                                      a-val
                                                                      (closureV-ds f-val))))]
        [ifexp (test-expr then-expr else-expr)    (if (interp test-expr ds) (interp then-expr ds) (interp else-expr ds))]))
 



;[test1] run: sexp, ds -> numV 
(define (run sexp ds)
     (if (equal? (is-recursion sexp) true)
         (interp (parse(desugar sexp)) ds)
         (interp (parse sexp) ds)))
;[test1]
(run '{with {count {fun {n} {if {= n 0} 0 {+ 1 {count {- n 1}}}}}}
                      {count 8}} (mtSub)
)
;[test2]
(run '{with {fib {fun {n}
                          {if {or {= n 0} {= n 1}}
                              1
                              {+ {fib {- n 1}}
                             {fib {- n 2}}}}}}
      {fib 10}} (mtSub))


