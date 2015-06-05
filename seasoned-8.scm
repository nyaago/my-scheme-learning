; kar, kdr, kons
; car, cdr, cons のようなもの
; kons は関数(kar, kdrが適用されることになる）を適用する関数を返し
; kar, kdrはそのkonsの返す関数を引数にとって適用する.
(define kons
  (lambda (kar kdr)
    (lambda (selector)　  
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

; kar, kdr, konsをつかって
; (egg egg ..) みたいなのを返す関数を返す lots(m) と
; 関数を引数にとってリストの長さを返す lenkth
(define lots 
  (lambda (m)
    (cond 
     ((zero? m) '())
     (else
      (kons 'egg (lots (- m 1)))))))

(define lenkth
  (lambda (l)
    (cond
     ((null? l) 0)
     (else
      (+ 1 (lenkth (kdr l)))))))




; kar, kdr, set-kar, bons
; car, cdr, cons のようなものとkdrに値を設定するset-kdr
; kons は関数(kar, kdr, set-kdrが適用されることになる）を適用する関数を返し
; kar, kdrはそのbonsの返す関数を引数にとって適用する.
; set-kdr もbonsの返す関数を引数にとって適用する.
 (define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda(s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda(s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda(s a d) s)) x)))

; 上のbons, set-kdr を使ってのkons
; 
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

  