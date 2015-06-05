(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define numbered?
  (lambda (aerp)
    (cond
     ((atom? aerp) (number? aerp))
     ((eq? (car (cdr aerp)) '×) 
      (and (numbered?  (car aerp)) 
           (numbered? (car (cdr (cdr aerp))))))
     ((eq? (car (cdr aerp)) '＋) 
      (and (numbered?  (car aerp)) 
           (numbered? (car (cdr (cdr aerp))))))
     ((eq? (car (cdr aerp)) '↑) 
      (and (numbered?  (car aerp)) 
           (numbered? (car (cdr (cdr aerp)))))))))


(define numbered?
  (lambda (aerp)
    (cond
     ((atom? aerp) (number? aerp))
     (else
      (and (numbered?  (car aerp)) 
           (numbered? (car (cdr (cdr aerp)))))))))



(define value
  (lambda (aerp)
    (cond
     ((atom? aerp) aerp)
     ((eq? (car (cdr aerp)) '×) 
      (* (value  (car aerp)) 
           (value (car (cdr (cdr aerp))))))
     ((eq? (car (cdr aerp)) '＋) 
      (+ (value  (car aerp)) 
           (value (car (cdr (cdr aerp)))))))))
