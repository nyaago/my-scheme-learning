(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))



(define eternity
  (lambda (x)
    (eternity x)))

; length0
(lambda (l)
  (cond
   ((null? l) 0)
   (else (+ 1 (eternity (cdr l))))))


; length1
(lambda (l)
  (cond
   ((null? l) 0)
   (else (+ 1 
            ((lambda (l)
              (cond 
               ((null? l) 0)
               (else (+ 1) (eternity (cdr l)))))
             (cdr l))))))

; length0
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l))))))) 
 eternity)


; length1
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (+ 1 (length (cdr l)))))))
  eternity))

; length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
   (cond
    ((null? l)  0)
    (else
     (+ 1 (length (cdr l))))))))


; length1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
   (cond
    ((null? l)  0)
    (else
     (+ 1 (length (cdr l))))))))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
   (cond
    ((null? l)  0)
    (else
     (+ 1 ((mk-length mk-length) (cdr l))))))))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else
          (+ 1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
      


((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l))))))))




(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


((Y 
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))) '(1 2))
 