(define (filter pred lis)
  (if (null? lis)
      (list )
      (if (pred (car lis) )
          (cons (car lis)  (filter pred  (cdr lis) ))
          (filter pred  (cdr lis)) )))


(define (tree-walk walker proc tree)
  (walker (lambda (elt)
            (if (list? elt)
                (tree-walk walker proc elt)
                (proc elt)))
          tree))



(define (numbers-only-for-tree walker)
  (lambda (proc lis)
    (walker proc (filter (lambda (x) (or (number? x) (pair? x))) lis))))

(print (tree-walk (numbers-only-for-tree map)
           (lambda (x) (* 2 x)) '(1 2 #f (3 4) #t 5 (6 (7 #t 8)))) )

 ( print ( tree-walk (numbers-only-for-tree map) (lambda (x) (* x 3 )) '(1 2 #f ( 5  #f 6)  3 4))  )