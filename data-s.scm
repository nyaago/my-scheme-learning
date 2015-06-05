(define stack '())
(define set-stack
  (lambda ()
    (set! stack '())))

(define make-stack
  '(lambda ()
     '()))

(define push
  (lambda (x stack)
    (set! stack (cons x stack))
    stack))

(define pop
  (lambda (stack)
    (let ((res '() ))
          (if (null? stack)
              '()
              (begin
                (set! res (car stack))
                (set! stack (cdr stack))
                res)))))
    

(define make-queue
  (lambda ()
    (cons '() '())))