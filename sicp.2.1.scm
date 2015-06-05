; exercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p) ) )


;(car (cons x y ))
;(car (lambda (m) (m x y)))
;(lambda (m x y) (lambda (p q) p ))
;((lambda (p q) p) x y)

(define (cdr z)
  (z (lambda (p q) q) ) )


(define zero (lambda (f) (lambda (x) x )))