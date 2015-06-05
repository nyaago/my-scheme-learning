(print 
(((lambda (y)
    (lambda (x)
      (begin
        (set! y (+ y #?=x))
        y)))
  10)
1)
)


(print ((lambda (x y)
(begin
   (- x y)
)
) 1 3 ))

