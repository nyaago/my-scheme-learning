(define (square x)
  (* x x)
)

(define (average a b) 
  (/ (+ a b) 2 )
)


;(define (good-enough? guess x)
;   ( <  #?=(abs (- ( square guess) x) )  0.0001)
;)

(define (improve guess x)
  (average (/ x guess) guess)
)

(define (sqrt-iter guess x )
  (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x)  x)
  )
)

(define (sqrt x)
  (sqrt-iter 1.0 x)
)

(define (good-enough? guess x)
  ( < #?=(abs (-  1.0  (/ (improve guess x) guess) )) 0.0001)
)





