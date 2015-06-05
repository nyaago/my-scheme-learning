;(define (reverse lis)
;  (fold  cons '() lis ) 
;)


;(define (append a b)
;  (if (pair? a)
;      (cons (car a) (append (cdr a) (if (pair? b) b (list b) )))
;      (if (null? a)
;          (if (pair? b) b (list b) )
;          (cons a (if (pair? b) b (list b) ))
;      )
;  )
;)


(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append (cdr a) b ))
      b
  )
)


;(define (push lis a)
;  (if (pair? lis)
;      (cons (car lis) (push (cdr lis)  a ))
;      (list a)
;      )
; )

(define (push lis a)
  (append lis (list a))
 )

      


(print (list (car '(1 3 6) ) ))
;(print (append 1 3 ) )
;(print (append 1 '(3 5)) )
;(print (append '(3 5) '(6 7)  ) )
;(print (append '(3 5) 6  ) )

(print (push '(3 5) 6  ) )
#;(define (reverse lis)
  (if (null? (cdr lis  ) )
      lis
      (append (reverse(cdr lis))  (car lis)  )
   )
)

(define (reverse lis);末尾再帰
  (define (reverse-rec lis a result)
    (if (null? lis)  
        result
        (reverse-rec (cdr lis) (car lis) (append  (list a) result   ) )
        )
    )
  (if (pair? lis)
      (reverse-rec (cdr lis) (car lis) '() )
      lis
      )
)



(print (reverse '(1 2 3 4 5 6) ) )




(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis)  (copy-list  (cdr lis ) ) 
       )
      lis
    )
)


(define (deep-copy-list lis)
  (if (pair? lis)
      (cons (deep-copy-list ( car lis) )  (deep-copy-list  (cdr lis ) ) 
       )
      lis
    )
)





(print  (copy-list '(1 2 3 4 5) ) )
(print  (deep-copy-list '(a (1 2 3) (4 5) c ) ) )
(print  (copy-list '(a (1 2 3) (4 5) c ) ) )
