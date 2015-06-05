(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))


; tableをから要素を得る
; table => テーブル.名前を指定すると値を返す関数
(define lookup
  (lambda (table name)
    (table name)))


; tableの拡張, tableの値を返す関数を拡張する。
; name1 => テーブルから値を取り出すキーとなる名前
; value => テーブルに設定する値.
; table => テーブル.名前を指定すると値を返す関数
; 名前を指定すると値を返すtable(関数)を返す。
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (if (eq? name1 name2 )
          value
          (table name2)))))
          

;
(define define?
  (lambda (e)
    (cond
     ((atom? e) #f)
     ((atom? (car e))
      (eq? (car e) (quote define)))
     (else #f))))
      
; define のようなもの
; table に　名前と関数の組のboxを追加する。
(define *define
  (lambda (e)
    (set! global-table
          (extend
           (name-of e)
           (box
            (the-meaning
             (right-side-of e)))
           global-table))))

; box をつくる
; 値を指定して値を保持する関数を返す。
; 返された関数は適用するselector関数を引数にとる。
; selectorはboxに保持されている値とlambda式(<-値を設定するための)を引数にとる関数。
(define box
  (lambda (it)
    (lambda (selector)
      (selector it (lambda (new)
                (set! it new))))))

; box の内容を変更 
; box(box 関数で返された関数)と新しい値を引数として指定、指定された関数の結果値を新しい値に変更する。
; (box 関数が返す関数の (lambda (new)
;                (set! it new)) の部分の適用により新しい値が設定される )
; boxにselector となる lambda 式を渡すことにより実現
; box =>
; new => 新しい値
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

; box の内容を取り出す
; box(box 関数で返された関数)を引数として指定、関数の結果値を返す
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

; '式'を引数にとって、実行される関数を実行
(define the-meaning
  (lambda (e)
    (meaning e looking-in-global-table)))


; globalテーブルより 指定した名前のテーブルを返す
(define looking-in-global-table
  (lambda (name)
    (lookup global-table name)))

; '式'とテーブルを引数にとって、関数を実行
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


;;
;;
(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifer
  (lambda (e table)
    (unbox (lookup table e))))


(define *set
  (lambda (e table)
    (setbox 
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extended
               (formals-of e)
               (box-all args)
               table)))
))

; lambda の関数定義を適用
; lambda の結果を返す
; es => 関数定義部分のS式
; table => 
(define beglis
  (lambda (es table)
    (cond
     ((null? (cdr es))
      (meaning (car es) table))
     (else ((lambda (val)
              (beglis (cdr es) table))
            (meaning (car es) table))))))

; 指定されたリストの要素(<-lambda式 への引数)のboxをつくり、それをリストを返す
(define box-all
  (lambda (vals)
    (cond
     ((null? vals) (quote ()))
     (else
      (cons (box ( car vals))
            (box-all (cdr vals)))))))


; 名前のリスト、値のリストをとってテーブルを拡張
(define multi-extended
  (lambda (names values table)
    (cond
     ((null? names) table)
     (else
      (extend (car names) 
              (car values)
              (multi-extended
               (cdr names)
               (cdr values)
               table))))))



; 奇数/偶数判定
(define odd?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else
      (even? (sub1 n))))))

                
(define even?
  (lambda (n)
    (cond
     ((zero? n) #t)
     (else
      (odd? (sub1 n))))))

; 関数の適用
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

; 式のリストからそれぞれの値のリストを返す
(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      ((lambda (val)
         (cons val
               (evlis (cdr args) table)))
       (meaning (car args) table))))))



; 関数を適用する関数を返す、１引数のもの
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

; 関数を適用する関数を返す、２引数のもの
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list) 
         (car (cdr args-in-a-list ))))))

; 
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     ((eq? e (quote cons))
      (b-prim cons))
     ((eq? e (quote car))
      (a-prim car))
     ((eq? e (quote cdr))
      (a-prim cdr))
     ((eq? e (quote eq?))
      (b-prim eq?))
     ((eq? e (quote atom?))
      (a-prim atom?))
     ((eq? e (quote null?))
      (a-prim null?))
     ((eq? e (quote zero?))
      (a-prim zero?))
     ((eq? e (quote add1))
      (a-prim add1))
     ((eq? e (quote sub1))
      (a-prim sub1))
     ((eq? e (quote number?))
      (a-prim number?))
)))


(define *const
  (let ((_cons (b-prim cons))
        (_car (a-prim car))
        (_cdr (a-prim cdr))
        (_null? (a-prim null?))
        (_zero? (a-prim zero?))
        (_eq? (b-prim eq?))
        (_atom? (a-prim atom?))
        (_add1 (a-prim add1))
        (_sub1 (a-prim sub1))
        (_number? (a-prim number?)))
    (lambda (e table)
      (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       ((eq? e (quote cons)) _cons)
       ((eq? e (quote car)) _car)
       ((eq? e (quote cdr)) _cdr)
       ((eq? e (quote eq?)) _eq?)
       ((eq? e (quote zero?)) _zero?)
       ((eq? e (quote null?)) _null?)
       ((eq? e (quote atom?)) _atom?)
       ((eq? e (quote add1)) _add1)
       ((eq? e (quote sub1)) _sub1)
       ((eq? e (quote number?)) _number?)
))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines))))
    ((meaning (question-of (car lines)))
     (meaning (answer-of (car lines))))
    (else
     (evcon (cdr lines) table))))

(define *letcc
  (lambda (e table)
    (let/cc skip
            (beglis (ccbody-of e)
                    (extend
                     (name-of e)
                     (box (a-prim skip))
                     table))
)))

(define abort2 #f)
(define value
  (lambda (e)
    (let/cc the-end
            (set! abort2 the-end)
            (cond
             ((define? e) (*define e))
             (else
              (the-meaning e))))))


(define the-empty-table
  (lambda (name)
    (abort2
     (cons (quote no-answer)
           (cons name (quote ()))))))


; 式から実行するアクションを返す
(define expression-to-action
  (lambda (e)
    (cond 
     ((atom? e) (atom-to-action e))
     (else
      (list-to-action e)))))


(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons )) *const)
     ((eq? e (quote car )) *const)
     ((eq? e (quote cdr )) *const)
     ((eq? e (quote eq? )) *const)
     ((eq? e (quote null? )) *const)
     ((eq? e (quote zero? )) *const)
     ((eq? e (quote atom? )) *const)
     ((eq? e (quote add1 )) *const)
     ((eq? e (quote sub1 )) *const)
     ((eq? e (quote number? )) *const)
     (else *identifer))
))


(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond 
       ((eq? (car e) (quote quote))
        *quote)
       ((eq? (car e) (quote lambda))
        *lambda)
       ((eq? (car e) (quote letcc))
        *letcc)
       ((eq? (car e) (quote set!))
        *set)
       ((eq? (car e) (quote cond))
        *cond)
       (else
        *application)))
     (else
      *application))))
 



; 各式の要素を返す
(define text-of
  (lambda (x)
    (car (cdr x))))
(define formals-of
  (lambda (x)
    (car (cdr x))))
(define body-of
  (lambda (x)
    (cdr (cdr x))))
(define cctext-of
  (lambda (x)
    (cdr (cdr x))))
(define name-of
  (lambda (x)
    (car (cdr x))))
(define right-side-of
  (lambda (x)
    (cond
     ((null? (cdr (cdr x))) 0)
     (else (car (cdr (cdr x)))))))
(define cond-lines-of
  (lambda (x)
    (cdr x)))
(define else?
  (lambda (x)
    ((atom? x) (eq? x (quote else)))
    (else #f)))
(define question-of
  (lambda (x)
    (car x)))
(define answer-of
  (lambda (x)
    (car (cdr x))))
(define function-of
  (lambda (x)
    (car x)))
(define arguments-of
  (lambda (x)
    (cdr x)))

;
(define global-table
    the-empty-table )

