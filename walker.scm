(use srfi-1)

(define (tree-walk walker proc tree)
  (walker (lambda (elt)
            (if (list? elt)
                (tree-walk walker proc elt)
                (proc elt)))
          tree))

(define (filter-for-tree pred lis)
  (filter (lambda(x)  (or (pred x) (pair? x )) ) lis)
)


(define (map-numbers pred lis)
  (map pred
    (filter number? lis)
  )
)


(define (for-each-numbers pred lis)
  (for-each pred
    (filter number? lis)
  )
)

;(print (map-numbers (lambda(x) (* x 2) ) '(1 2 #t 3 4) ) )
;(map-numbers (lambda(x) ( print x ) ) '(1 2 #t 3 4) ) 



(define (numbers-only walker)
  (lambda (proc lis)
    (walker proc (filter number? lis)))
)
;(print ( (numbers-only map)  (lambda(x) (* x 2) ) '(1 2 #f 4 5) ) )



(define (numbers-only-for-tree walker)
  (lambda (proc lis)
    (walker proc (filter (lambda(x) (or (number? x) (pair? x) )   ) lis ) ) )
)

(define (numbers-only-for-tree walker)
  (lambda (proc lis)
    (walker proc (filter-for-tree number? lis)))
)

(define (filtered-walker-for-tree walker pred)
  (lambda (proc lis)
    (walker proc (filter-for-tree pred lis)))
)



; (print ( tree-walk ( numbers-only-for-tree map) (lambda (x) (* x 2 )) '(1 2  3 4) ) )
; ( print ( tree-walk (numbers-only-for-tree map) (lambda (x) (* x 3 )) '(1 2 #f ()  ( 5  #f 6 (7 8) )  9 10))  )


; ( print ( tree-walk (filtered-walker-for-tree map number?) (lambda (x) (* x 3 )) '(1 2 #f ()  ( 5  #f 6)  3 4))  )


; ( print ( tree-walk 
;    (filtered-walker-for-tree map (lambda(x) (and (number? x) (even? x) ))) 
;    (lambda (x) (* x 3 )) '(1 2 #f ()  ( 5  #f 6)  3 4))  )


