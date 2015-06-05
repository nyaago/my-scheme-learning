
;(print (write-to-string  '(1 "abc" "\"" #\z )))
(define (write-to-string  x )
  (call-with-output-string 
   (lambda (out)  (write x out) )
   )
)

(define (read-from-string x)
  (call-with-input-string x
     (lambda (in)  (read  in) )
   )
)

;; 
;(print (write-to-string  '(1 "abc" "\"" #\z )))
;(define x (write-to-string  '(1 "abc" "\"" #\z )))
;(print (read-from-string x) )




(define (write-tree tree port . separator)
  (cond [(pair? tree)
         (write-tree (car tree) port )
         (write-tree (cdr tree)  port)  ]
        [(null? tree)]  
        [else (display tree port)] ;要素
        )
)


(define (tree->string tree)
  (call-with-output-string 
   (lambda (out)  (write-tree tree out) )
   )
 
)



;(define t (cons (cons "cat" "dog") "bear") )
;(write-tree t (current-output-port))


;(print (write-to-string  '(1 2 3 )))
;(define x (write-to-string  '(1 2 3 )))
;(print (read-from-string x) )


