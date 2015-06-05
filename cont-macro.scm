(define-syntax for-each-ext 
  (syntax-rules ()
    [ (_  break next expr lis)
      (call/cc (lambda (break) 
                 (for-each 
                  (lambda (x)
                    (call/cc (lambda (next) (expr x) ) )
                 )
                  lis) ) )
     ] ) )


(for-each-ext break next
 (lambda (x) 
   (if (> x 3) 
       (break #t) 
       (print x) 
              ) )  
 '(1 2 3 4 5)
)


(for-each-ext break1 next1
 (lambda (x) 
   (if (odd? x ) 
       (next1 #f) 
       (print x)  ) )
 '(1 2 3 4 5)
)



(for-each-ext
 (lambda (x) 
   (if (odd? x ) 
       (next #f) 
       (print  x) ) )
 '(1 2 3 4 5)
)
