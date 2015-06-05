(use srfi-1)


(define (member elt lis . options)
  (let-optionals* options ( (pred equal? ))
    (cond 
     [(null?  lis) #f ] 
     [(pred elt (car lis) ) lis]
     [else (member elt (cdr lis) pred) ]
    )
  )   
)


(define *categories* (list 'dog  'cat 'tiger) )
(print (member 'cat *categories* equal?  ))




(define (assoc key alist . options)
  (let-optionals* options ((pred equal? ))
     (cond 
      [(null? alist) #f]
      [(pred key (car (car alist)) ) (car alist)   ]
      [else (assoc key (cdr alist) pred ) ]
     )
  )
)

(define (assoc key alist . options)
  (let-optionals* options ((pred equal? ))
    (define (loop alist)
      (cond 
       [(null? alist) #f]
       [(pred key (car (car alist)) ) (car alist)   ]
       [else (loop (cdr alist) ) ]
       )
     )
    (loop alist)
  )
)


(define *categories* (list '(dog . 1)  '(cat . 2) '(tiger . 3)  ) )
(print (assoc 'cat *categories*) )


