#lang plai
;type def
(define-type LFAE
    [num    (n number?)]
    [add     (lhs LFAE?) (rhs LFAE?)]
    [sub     (lhs LFAE?) (rhs LFAE?)]
 
    [id         (name symbol?)]
    [fun      (param symbol?) (body LFAE?)]
    [app     (ftn LFAE?) (arg LFAE?)])
    ;[if (=(lhs FAE?) (rhs FAE?)])

; strict , num-op
(define (num-op op x y)
    (numV (op (numV-n (strict x))
                        (numV-n (strict y)))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

; strict: LFAE-Value -> LFAE-Value
(define (strict v)
    (type-case LFAE-Value v
        [exprV (expr ds) (strict (interp expr ds))]
        [else v]))
; parse: sexp -> LFAE
; purpose: to convert sexp to FWAE ;add that if else condition => to solve the error of the syntax 
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)                     (add (parse l) (parse r))]
        [(list '- l r)                      (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)     (fun p (parse b))]  ;; e.g., {fun {x} {+ x 1}}
        [(list f a)                       (app (parse f) (parse a))]
        [else                             (error 'parse "bad syntax: ~a" sexp)]))


(define-type LFAE-Value
        [numV         (n number?)]
        [closureV    (param symbol?)
                             (body LFAE?)
                             (ds DefrdSub?)]
        [exprV          (expr LFAE?)
                              (ds DefrdSub?)])
(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

;[contract] lookup: symbol DefrdSub -> number
;[purpose]
(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

;interpreter
;[contract]interp: LFAE -> numV
;[purpose] interp: LFAE DefrdSub -> LFAE-Value
(define (interp lfae ds)
    (type-case LFAE lfae
        [num (n)          (numV n)]
        [add  (l r)         (num+ (interp l ds) (interp r ds))]
        [sub  (l r)         (num- (interp l ds) (interp r ds))]
        [id      (name)  (lookup name ds)]
        [fun   (param body-expr)  (closureV param body-expr ds)]
        [app   (f a)        (local [(define f-val (strict (interp f ds)))     ;ftn-v --> f-val , + strict()                     
                                                    (define a-val (exprV a ds))]  ; avg-v --> a-val              
                                             (interp (closureV-body f-val)
                                                          (aSub (closureV-param f-val)
                                                                      a-val
                                                                      (closureV-ds f-val))))]))

;[test-case]
;[contract] run : sexp -> numV
(define (run sexp ds)
     (interp (parse sexp) ds))
;[test1]
(run '{{fun {x} {+ 1 x}} 10} (mtSub))
;[test2]
(run '{{fun {f} {f 1}} {fun {x} {+ x 1}}} (mtSub))


