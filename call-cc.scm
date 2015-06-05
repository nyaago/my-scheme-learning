((lambda (x) (x (+ 3 5))) (lambda (k) (* 7 k)))

(* 7 (call/cc (lambda (x) (x (+ 3 5)))))

(define opp/cc #f)
(* 7 (call/cc (lambda (x) (set! opp/cc x)  (x (+ 3 5)))))

(* 3 5 (call/cc (lambda (x)  (set! opp/cc x)   (x 7))))
(* 3  (call/cc (lambda (x)  (set! opp/cc x)   (x 7))) 7)


(define product
  (lambda (lst)
    (call/cc 
     (lambda (k)
       (cond ((null? lst) 1)
             ((= (car lst) 0)  (k 0))
             (else (* (car lst) (product (cdr lst)))))))))