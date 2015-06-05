(define (find-fold pred? proc seed lis) 
  (cond [(null? lis) seed]
         [(pred? (car lis) )
          (let (
                (seed2 (proc (car lis) seed) ) )
            (find-fold pred? proc seed2  (cdr lis))) ]
         [else (find-fold pred? proc seed (cdr lis))]
  ) ; cond 
)

(define (process elt seed)
  (print "found: " elt)
  (cons elt seed)
)


(define (find-fold2 pred? proc/cont seed lis) 
  (cond [(null? lis) seed]
         [(pred? (car lis) )
                 (proc/cont (car lis) 
                            seed
                            (lambda (seed2) 
                              (find-fold2 pred? proc/cont seed2 (cdr lis) )
                              ))]
         [else (find-fold2 pred? proc/cont seed (cdr lis))]
  ) ; cond 
)


(define (process/cont elt seed cont)
  (print cont)
  (cont (cons elt seed) )
)


(print (find-fold2 odd? process/cont '() '( 2 3 4 5 6 7 8)) )


(define next #f)
(define (break  val val) )
(define (process/break elt seed cont)
  (set! next 
    (lambda ()
      (print "found: " elt)
      (cont (cons elt seed) )
      ) )
    (break #f)
)




(define (find-fold/cont pred? proc/cont seed lis cont) 
  (cond [(null? lis) (cont seed)]
         [(pred? (car lis) )
                 (proc/cont (car lis) 
                            seed
                            (lambda (seed2) 
                              (find-fold/cont pred? proc/cont seed2 (cdr lis) cont)
                              ))]
         [else  (find-fold pred? proc/cont seed (cdr lis) cont)]
  ) ; cond 
)


(define (process/cc elt seed)
  (call/cc
   (lambda (cont)
     (print "found: " elt)
     (cont (cons elt seed)))
   )   
)


(define (breaker/cc proc break)
  (lambda (elt seed)
    (call/cc 
     (lambda (cont)
       (set! next (lambda() (cont (proc elt seed))))
       (break #f) ) )  ) 
)



(find-fold odd? process/cc '()  '(1 2 3 4 5 6 7 8) )

(call/cc 
 (lambda (cont0)
   (find-fold odd? (breaker/cc process cont0)
              '() '(1 2 3 4 5 6 7 8) ) ) ) 

(print (next))
(print (next))