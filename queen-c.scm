(define non 
  (lambda (x) 'non))


(define id
  (lambda (x) x))

(define add1 
  (lambda (n) (+ n 1)))
(define sub1
  (lambda (n) (- n 1)))


(define ++ 
  (lambda (n) (+ n 1)))
(define --
  (lambda (n) (- n 1)))
(define **
  (lambda (a b) (expt a b)))

(define //
  (lambda (a b) (quotient a b)))
(define /@
  (lambda (a b) (remainder a b)))
(define /:
  (lambda (a b) (modulo a b)))

(define make-even
  (lambda (n) (* n 2)))

(define make-odd
  (lambda (n) (+ (* n 2) 1)))

; odd -> -1, evne -> 1
(define parity-of
  (lambda (p)
    (if (odd? p) -1 1)))

 (define adjust-of
   (lambda (x)
     (let ((digit 1000)
            (slide (if (positive? x) 1/2 -1/2)))
        (/ (truncate (+ slide (* digit x))) digit))))