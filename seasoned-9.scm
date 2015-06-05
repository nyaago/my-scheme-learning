
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


; 最初のatomを返すための関数
; leaveに継続が設定されることにより機能する
(define leave #f)

(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (leave (car l)))
     (else
      (let ()
        (walk (car l))
        (walk (cdr l)))))))

; 最初のatomを返すための関数
; leaveに継続を設定することによりwalkから値が返される.
(define start-it
  (lambda (l)
    (let/cc here
            (set! leave here)
            (walk l))))


; 
(define fill #f)
(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (let ()
        (let/cc rset
                (set! fill rset)
                (leave (car l)))
        (waddle (cdr l)))))
     (else
      (let ()
        (waddle (car l))
        (waddle (cdr l))))))

(define start-it2
  (lambda (l)
    (let/cc here
            (set! leave here)
            (waddle l))))


(define get-next
  (lambda (l)
    (let/cc here-again
            (set! leave here-again)
            (fill '()))))

(define get-first
  (lambda (l)
    (let/cc here
            (set! leave here)
            (waddle l)
            (leave '()))))
    
