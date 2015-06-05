(define-syntax nand
  (syntax-rules ()
    ((_) 
     #f)
    ((_ p q ...) 
     (not (and p q ...)))))



(define-syntax nand
  (syntax-rules ()
    ((_) 
     #f)
    ((_ p) 
     (if p #f #t))
    ((_ p q ...) 
     (if p (nand q ...) #t ))))


(define-syntax inc
  (syntax-rules ()
    ((_ i) 
     (begin (set! i (+ i 1)) i))
    ((_ i k)
     (begin (set! i (+ i k)) i))))


(define-syntax dec
  (syntax-rules ()
    ((_ i)
     (begin (set! i (- i 1)) i))
    ((_ i k)
     (begin (set! i (- i k)) i))))


