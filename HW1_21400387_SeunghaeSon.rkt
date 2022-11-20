#lang plai

;Problem1:
;Solved by myself: Y
;time Taken: 1 mins
;[contract] dollar -> won : number -> number
;[purpose] To convert dollar to won (korean dollar)
;[tests](test(dollar->won 10) 13420)
;(test(dollar->won 5) 6710)
(define (dollar->won a)
         (* a 1342))

(test(dollar->won 10) 13420)
(test(dollar->won 5) 6710)

;Problem2:
;Solved by myself: Y
;time Taken: 1 mins
;[contract] take three numbers -> sum of numbers : numbers -> numbers
;[purpose] To sum of three numbers
;[tests](test(digit_num 1 2 3) 6)
;(test(digit_num 1 2 3) 7)
(define (digit_num a b c)
         (+ a b c))

(test(digit_num 1 2 3) 6)
(test(digit_num 1 2 4) 7)

;Problem3:
;Solved by myself: Y
;time Taken: 5 mins
;[contract] radius -> sphere:  number -> number(float)
;[purpose] To get volume of sphere
;[tests](test(volume-sphere 3) 11348)
;(test(volume-sphere 2) 5)
(define (volume-sphere b)
  (* 4/3 pi (* b b b)))

(test(volume-sphere 2) 33.5)
(test(volume-sphere 3) 113)

;Problem4:
;Solved by myself: Y
;time Taken: 20 mins
;[contract] integer -> integer or string(boolean) 
;[purpose] To distinguish even numbers
;[tests](test(is-even? 3) "is not even")
;(test( is-even? 2) 2)
(define (is-even? a)
  (cond
    [(= (modulo a 2) 0) a]
    [else "is not even"]
    )
  )

(test(is-even? 2) 2)
(test(is-even? 3) "is not even")

;Problem5:
;Solved by myself: Y
;time Taken: 120 mins
;[contract] integer -> integer
;[purpose] To get combination of integers 
;[tests](test(combination 3 2) 3)
;(test(combination 10 2) 45)

(define (combination n k)
  (cond
    [(= k 1) (/ n k)]
    [else (* (/ n k) (combination (- n 1) (- k 1)))]
    )
  )


(test(combination 3 2) 3)
(test(combination 10 2) 45)

;Problem6-a:
;Solved by myself: Y
;time Taken: 60 mins
;[contract] Bicycle -> wheels, Car -> wheels, windows, Airplane -> wheels, windows,engines
;[purpose] To define the type Vehicles.

(define-type Vehicle
    [Bicycle (wheels number?)]
    [Car (wheels number?)
         (windows number?)]
  [Airplane (wheels number?)
            (windows number?)
            (engines number?)]
  )

;problem6-b:
;Solved by myself: Y
;time Taken: 120 mins
;[contract] vehicle's attributes -> tax : numbers -> number
;[purpose] Taxing Vehicles. 
;[tests](test (tax-vehicle vehicle1) 4)
;(test (tax-vehicle vehicle3) 5)

(define vehicle1(Bicycle 4))
(define vehicle2(Car 4 7))
(define vehicle3(Airplane 0 3 2))

(define (tax-vehicle vehicle)
  (type-case Vehicle vehicle
    [Bicycle (wheels) wheels]
    [Car (wheels windows) (+ wheels windows)]
    [Airplane (wheels windows engines) (+ wheels windows engines)])
  )

(test (tax-vehicle vehicle1) 4)
(test (tax-vehicle vehicle3) 5)

;problem6-c:
;Solved by myself: Y
;time Taken: 12 mins
;[contract] vehicle's attributes -> safe or unsafe : number -> string
;[purpose] Setting the safety of vehicles standards.
;[tests](test (is-vehicle-safe vehicle1) "safe")
;(test (is-vehicle-safe vehicle3) "unsafe")

(define (is-vehicle-safe vehicle)
  (type-case Vehicle vehicle
       [Bicycle (wheels)
                (cond
                   [(>= wheels 4) "safe"]
                       [else "unsafe"])]
       [Car (wheels windows)
            (cond
              [(and (>= wheels 3) (>= windows 3)) "safe"]
                 [else "unsafe"])]
       [Airplane (wheels windows engines)
                (cond
                  [(and (>= wheels 2) (>= windows 10) (>= engines 1)) "safe"]
                 [else "unsafe"])]
    )
)

(test (is-vehicle-safe vehicle1) "safe")
(test (is-vehicle-safe vehicle3) "unsafe")

;problem7:
;Solved by myself: Y
;time Taken: 60 mins
;[contract] a -> b : string -> string 
;[purpose] Replacing old symbols to new symbols.
;[tests](test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
;(test (update-name 'flower 'Kid (cons 'Cath (cons 'flower (cons 'sons empty)))) '(Cath Kid sons))

(define (update-name a b list)
  (cond
    [(empty? list) empty]
    [(symbol=? (first list) b)
     (append (cons a empty) (update-name b a (rest list)))]
    [else
     (append (cons (first list) empty) (update-name b a (rest list)))]
    )
  )
  
(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test (update-name 'flower 'Kid (cons 'Cath (cons 'flower (cons 'sons empty)))) '(Cath Kid sons))

;problem8:
;solved by myself: N (time attacked!)
;time Taken: 30 mins
;[contract]
;[purpose] Sorting numbers
;[tests]
;