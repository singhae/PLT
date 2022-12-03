#lang plai
(define (ifexp sexp)
  (cond
    [(eq? (first sexp) (second sexp) (ifexp (first sexp)))]))
;(list 'ifexp l r) (if (parse l) (parse r)) 
(define (ifexp x y) (if x y)) 
          
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)])

; [Contract] parse: sexp -> AE
; [Purpose] To convert s-expressions into AEs in abstract syntax

(define (parse sexp)
	(cond
		[(number? sexp) (num sexp)]
		[(eq? (first sexp) '+) (add (parse (second sexp))
						(parse (third sexp)))]
		[(eq? (first sexp) '-) (sub (parse (second sexp))
						(parse (third sexp)))]
         )
)

;[contract] interp: AE -> number
;[purpose] consumes an AE and computes the corresponding number.

(define (interp an-ae)
	(type-case AE an-ae
		[num (n) n]
		; n is recognized as an actual number for computers.
		[add (l r) (+ (interp l) (interp r))]
		; add is recognized as a real behavior to sum two AEs.
		[sub (l r) (- (interp l) (interp r))]
		; sub is recognized as a real behavior to subtract two AEs.
	)
)

(test(parse '{- 2 1}) (sub (num 2) (num 1)))

(define (run sexp ds)
    (interp (parse sexp) ds))
;(run '{{fun {x} {+ 1 x}} 10} (mtSub))
;(run '{- 2 1})
