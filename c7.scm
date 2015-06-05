(define zero (lambda (s) (lambda (z) z)))
(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s (s z)))))
(define three (lambda (s) (lambda (z) (s (s (s z))))))


(define Succ
  (lambda (v)
    (lambda (s)
      (lambda (z)
        (s ((v s) z))))))


(define Plus
  (lambda (u v)
    (lambda (s)
      (lambda (z)
        ((u s) ((v s) z))))))


(define Mult
  (lambda (u v)
    (lambda (s)
      (lambda (z)
        ((u (v s)) z)))))


(define *true
  (lambda (x)
    (lambda (y)
      x)))

(define *false
  (lambda (x)
    (lambda (y)
      y)))

(define *if
  (lambda (c m n)
    ((c m) n)))

; Yコンビネータ
(define Y
  (lambda (X)
    ((lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg)))))))

; Yコンビネータ　を使って階乗
(define F*
  (lambda (func-arg)
    (lambda (n)
      (if (zero? n)
          1
          (* n (func-arg (- n 1)))))))
((Y F*) 5)
; Yコンビネータ　を使って階乗
((Y (lambda (f) 
      (lambda (n)
        (if (zero? n)
            1
            (* n (f (- n 1))))))) 5)


; 階乗
(define Fact
  (lambda (f) 
    (lambda (n)
      (if (zero? n) 
          1
          (* n ((f f) (- n 1)))))))
;((Fact Fact) 5)    

; 階乗
(((lambda (f)
          (lambda (n)
            (if (zero? n)
              1
              (* n ((f f) (- n 1))))))
  (lambda (f)
    (lambda (m)
       (if (zero? m)
           1
           (* m ((f f) (- m 1))))))) 5)
 

(((lambda (x) (lambda (s) ((x x) s))) f) (- y 1))
