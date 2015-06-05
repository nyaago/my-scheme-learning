(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (cons (deep (- n 1))
              '()))))



(define Rs '())
(define Ns '())
(define deepR
  (lambda (n)
    (let ((result (deep n)))
    (set! Ns  (cons n Ns))
    (set! Rs (cons result Rs))
    result)))


(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (if (= n (car ns))
                (car rs)
                (A (cdr ns) (cdr rs))))))
      (A Ns Rs))))


(define member?
  (lambda (a lat)
    ((letrec
        ((yes? 
          (lambda (lat)
            (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else
              (yes? (cdr lat)))))))
      yes?) lat)))

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n ))))


(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns  (cons n Ns))
          (set! Rs (cons result Rs))
          result))))

    
(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (cons (deepM (- n 1))
              '()))))


(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Ns  (cons n Ns))
            (set! Rs (cons result Rs))
            result)))))

;;; deepM 最終..
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond 
               ((null? ns)
                #f)
               (else
                (if (= n (car ns))
                    (car rs)
                    (A (cdr ns) (cdr rs))))))))
            (A Ns Rs))))


(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Ns  (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))


; Y適用順手続き的コンピネーた
(define Y! 
  (lambda (L)
    (let ((h (lambda (l)  (quote () ))))
      (set! h
            (L (lambda (arg)  (h arg))))
      h)))

; Y適用順手続き的コンピネーたでリストの長さ
(define L
  (lambda (length)
    (lambda (l)
      (cond 
       ((null? l) 0)
       (else
        (+ 1 (length (cdr l))))))))

(define length (Y! L))            

; Y適用順手続き的コンピネーたでリストの深さ
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define D
  (lambda (depth*)
    (lambda (l)
      (cond
       ((null? l) 0)
       ((atom? (car l))
        (depth* (cdr l)))
       (else
        (let ((d1 ( + (depth* (car l)) 1))
              (d2 (depth* (cdr l))))
          (if (> d1 d2)
              d1
              d2)))))))
(define depth* (Y! D))
; または
(define D
  (lambda (depth*)
    (lambda (l)
      (cond
       ((null? l) 0)
       ((atom? (car l))
        (depth* (cdr l)))
       (else
        (max ( + (depth* (car l)) 1)
              (depth* (cdr l))))))))

(define depth* (Y! D))

            
              
          
               

    
                  
         
               
         
      
