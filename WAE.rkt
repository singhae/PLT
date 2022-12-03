#lang plai
;type define
(define-type WAE
[num (n number?)]
[add (lhs WAE?) (rhs WAE?)]
[sub (lhs WAE?) (rhs WAE?)]
[with (name symbol?) (named-expr WAE?) (body WAE?)]
[id (name symbol?)])

;parser
;[contract] parse: sexp - WAE
;[purpose] to convert s-expresion into WAE
(define (parse sexp)
(match sexp
	[(? number?) (num sexp)]
	[(list '+ l r) (add (parse l) (parse r))]
	[(list '- l r) (sub (parse l) (parse))]
	[(list 'with (list i v) e) (with i (parse v) (parse e))]
	[(? symbol?) (id sexp)]
         [else (error 'parse "bad syntax: ~a" sexp)]))

(define-type DefrdSub
            [mtSub]
            [aSub      (name symbol?)
                            (value number?)
                            (saved DefrdSub?)])
;; subst : WAE symbol WAE → WAE 
;; substitutes second argument with third argument in first argument, 
;; as per the rules of substitution; the resulting expression contains 
;; no free instances of the second argument 

(define (subst expr sub-id val) 
(type-case WAE expr
[num (n) expr] 
[add (l r) (add (subst l sub-id val) (subst r sub-id val))] 
[sub (l r) (sub (subst l sub-id val) (subst r sub-id val))] 
[with (bound-id named-expr bound-body) 
(if (symbol=? bound-id sub-id) 
    expr 
    (with bound-id 
         named-expr 
         (subst bound-body sub-id val)))] 
 [id (v) (if (symbol=? v sub-id) val expr)]))

;interpreter
(define (interp wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp l) (interp r))]
		[sub (l r) (- (interp l) (interp r))]
		[with (i v e) (interp (subst e i (interp v)))]
		[id (s) (error 'interp "free identifier")]))

(test(interp (with 'x (num 5) (add (id 'x) (id 'x))) 5))
;(interp (parse '{double {double 5}}) (list (fundef 'double 'n (add (id 'n) (id 'n))))) -> f1wae
