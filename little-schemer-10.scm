(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))



(define build
  (lambda (x y)
    (cons x (cons y '()))))


(define new-entry
  (lambda (names values)
    (cons names (cons values '()))))


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))


(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) 
      (entry-f name))
     ((eq? (car names) name) 
      (car values))
     (else
      (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table
  (lambda (entry table)
    (cons entry table)))
           

(define lookup-in-table 
  (lambda (name table table-f)
    (cond 
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name 
                        (car table)
                        (lambda (name)
                          (lookup-in-table name
                                            (cdr table)
                                            table-f)))))))


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
    ((eq? e 'cons) *const)
    ((eq? e 'car) *const)
    ((eq? e 'cdr) *const)
    ((eq? e 'null?) *const)
    ((eq? e 'eq?) *const)
    ((eq? e 'atom?) *const)
    ((eq? e 'zero?) *const)
    ((eq? e 'add1) *const)
    ((eq? e 'sub1) *const)
    ((eq? e 'number?) *const)
    (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond 
       ((eq? (car e) (quote quote))
        *quote)
       ((eq? (car e) 'lambda)
        *lambda)
       ((eq? (car e) 'cond)
        *cond)
       (else
        *application)))
     (else
      *application))))


(define value
  (lambda (e)
    (meaning e '())))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote () ))))
    


(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else 
      (build (quote primitive) e)))))



(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))    
  

(define table-of first)
(define formals-of second)
(define body-of third)

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else) )
     (else #f))))
 
(define question-of first)
(define answer-of second) 

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else
      (evcon (cdr lines) table)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table )))

(define cond-lines-of cdr)


(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning  (car #?=args) table) 
            (evlis (cdr args) table))))))


(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of #?=e) table))))


(define function-of car)
(define arguments-of cdr)


(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? #?=fun) 
      (apply-primitive (second fun)   vals))
     ((non-primitive? fun)
      (apply-closure 
       (second fun) vals)))))


(define apply-primitive
  (lambda (name vals)
  (print "apply-promitive name=" name) ;
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'atom?)
      (atom? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals) )))))

(define apply-closure
  (lambda (closure vals)
    (print "apply-closure - body =" (body-of closure));
    (meaning (body-of closure) 
             #?=(extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))
           
     

    



