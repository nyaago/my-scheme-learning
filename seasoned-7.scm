(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define find 
  (lambda (n Ns Rs)
    (cond 
     ((null? Ns) #f)
     ((eq? n (car Ns))  (car Rs))
     (else
      (find n (cdr Ns) (cdr Rs))))))

;
(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (- m 1)) '()  )))))
    (lambda (n)
      (let ( (exists  (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))
       
; 1つのlet にまとめる
(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (- m 1)) '()  )))))
    (lambda (n)
      (let ( (exists  (find n Ns Rs)))
        (if (atom? exists)
            (let ((result 
                   ((lambda (m)
                      (if (zero? m)
                          'pizza
                          (cons (deepM (- m 1)) '()  )))
                           n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))

; 関数名をつけないで
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ( (exists  (find n Ns Rs)))
        (if (atom? exists)
            (let ((result 
                   (if (zero? n)
                       'pizza
                       (cons (deepM (- n 1)) '()  ))))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))
       

;;;
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (+ N 1))
      (cons x y))))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (- m 1))
               '()))))


(define counter #f)
(define set-counter #f)


(define consC
  (let ((N 0))
    (set! set-counter (lambda () set! N 0))
    (set! counter (lambda () N))
    (lambda (x y)
      (set! N (+ N 1))
      (cons x y))))

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (- n 1)))))))
      (S 1000)
      (counter))))


(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (- m 1)) '()  )))))
    (lambda (n)
      (let ( (exists  (find n Ns Rs)))
        (if (atom? exists)
            (let ((result 
                   ((lambda (m)
                      (if (zero? m)
                          'pizza
                          (consC (deepM (- m 1)) '()  )))
                           n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))




(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l)
                (oh #f))
               ((atom? (car l))
                (if (eq? (car l) a)
                    (R (cdr l) oh)
                    (consC (car l) (R (cdr l) oh))))
               (else  ; not atom
                (let ((new-car
                       (let/cc oh
                               (R (car l) oh))))
                  (if (eq? new-car #f)
                      (consC (car l) (R (cdr l) oh))
                      (consC new-car (cdr l)))))))))
      (let ((new-l
             (let/cc oh
                     (R l oh))))
        (if (eq? new-l #f)
            l
            new-l)))))

                      
                               
                        
                
                
                    
                       
                