#lang plai
;type def
;+if else
(define-type FAE
    [num    (n number?)]
    [add     (lhs FAE?) (rhs FAE?)]
    [sub     (lhs FAE?) (rhs FAE?)]
 
    [id         (name symbol?)]
    [fun      (param symbol?) (body FAE?)]
    [app     (ftn FAE?) (arg FAE?)])
; num+: FWAE FWAE -> FWAE
(define (num+ x y)
     (num (+ (numV-n x) (numV-n y))))
; num-: FWAE FWAE -> FWAE
(define (num- x y)
     (num (- (numV-n x) (numV-n y))))
; parse: sexp -> FWAE
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

(define-type FAE-Value
    [numV         (n number?)]
    [closureV    (param symbol?) (body FAE?) (ds DefrdSub?)]) ;avoid ds. 

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])
; lookup: symbol DefrdSub -> number
(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

;interpreter
; interp: FWAE -> FWAE
; interp: FAE DefrdSub -> FAE-Value
;+if else
(define (interp fae ds)
    (type-case FAE fae
        [num   (n)      (numV n)]
       [add    (l r)    (num+ (interp l ds) (interp r ds))]
       [sub    (l r)    (num- (interp l ds) (interp r ds))]
       [id       (s)     (lookup s ds)]
       [fun     (p b)  (closureV p b ds)]
       [app    (f a)   (local [(define f-val (interp f ds))
                                      (define a-val (interp a ds))]
                               (interp (closureV-body f-val)
                                       (aSub (closureV-param f-val)
                                                          a-val
                                                          (closureV-ds f-val))))]))



(interp (parse '{with {mk-rec {fun {body-proc}
                                 {with {fX {fun {fY}
                                                         {with {f {fun {x}
                                                                       {{fY fY} x}}}
                                                                       {body-proc f}}}}
                                           {fX fX}}}}
           {with {fac {mk-rec
                                           {fun {fac}
                                                   ; Exactly like original fac
                                                   {fun {n}
                                                                 {if {= n 0}
                                                                        1
                                                                         {* n {fac {- n 1}}}}}}}}
                                    {fac 10}}}) (mtSub))

