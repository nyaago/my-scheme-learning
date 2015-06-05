; sicp
; Exercise 1.12 
; Pascal Triangle
(define (pascal-triangle level n )
  (cond ((= n 1) 1)       ; 左の辺
        ((= n level) 1)   ; 右の辺
        ((= n 0) 0)       ; 
        ((= level 0) 0)   ; 
        (else  (+ (pascal-triangle (- level 1)  n ) 
                  (pascal-triangle (- level 1) (- n 1))))))



; Exercise 1,11
; f(n) = n if n < 3 and f(n - 1) + 2f(n - 2) + 3f(n - 3)


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1) ) (* 2 (f (- n 2) ))  (* 3 (f (- n 3)) ) ) )
)

(define (f2 n)
  (f-iter 2 1 0 n)
)


(define (f-iter nm1 nm2 nm3 n)
  (cond ((< n 3) nm1)
        (else (f-iter (+ nm1 (* nm2 2) (* nm3 3) ) nm1 nm2 (- n 1)) ) ) )
               
