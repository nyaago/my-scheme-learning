(define (exponent b n)
  (if (= n 0) 
      1
      (* b  (exponent  b  (- n 1) ) ) )
)


(define (exponent b n)
  (exponent-iter b n 1)
)


(define (exponent-iter b n result)
  (if (= n 0)
      result
      (exponent-iter b (- n 1)  (* result b) )
) )

(define (square n)
  (* n n)
)


(define (exponent b n) 
  (cond ((= n 0) 1)
        ((even? n) (square  (exponent b (/ n 2)  )   ) )
        (else   (* b (exponent b (- n 1)  )
) ) ) )



(define (double n)
  (+ n n)
)
(define (halve n)
  (/ n 2)
)



(define (multi a b)
  (cond ((= b 0)  0 )
        ((even? b) (double (multi a (halve b) ) ) )
        (else (+ a (multi a (- b 1) )  ))
))



(define (multi a b)
  (multi-iter a b 0 )
)
(define (multi-iter a b r)
  (cond ((= b 0) r )
        ((even? b) (multi-iter (double a) (halve  b)  r ) )
        (else (multi-iter a (- b 1) (+ r a) ) )
))
