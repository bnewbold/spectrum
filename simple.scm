
; ### preliminaries, utilities
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (p) (car p)))

(define second 
  (lambda (p) (car (cdr p))))

(define build
  (lambda (a b) (cons a (cons b (quote ())))))

(define third
  (lambda (p) (car (cdr (cdr p)))))

(define *const
  (lambda (e table) 
    (cond          
     ((number? e) e) 
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))


(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name) (car values))
     (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;(lookup-in-entry 'fish
;		 '((teach a man to fish)
;		   (1 2 3 4 5))
;		 (lambda (x) x))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name 
			    (car table)
			    (lambda (n)
			      (lookup-in-table n (cdr table) table-f)))))))

;(lookup-in-table 'fish
;		 (extend-table '((teach a man to fish)
;				 (1 2 3 4 5))
;			       (quote ()))
;		 (lambda (x) x))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

;(atom-to-action 'number?); *const

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e)) (cond
		       ((eq? (car e) (quote quote)) *quote)
		       ((eq? (car e) (quote lambda)) *lambda)
		       ((eq? (car e) (quote cond)) *cond)
		       (else *application)))
     (else *application))))

;(list-to-action '(lambda (x) x)) ; *lambda
;(list-to-action '(cond ((eq? 1 2) #f) (else #t))) ; *cond

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

;(expression-to-action '#f) ; *const
;(expression-to-action '(lambda (x) x)) ; *lambda

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;(*const 'asdf '()) ; (primitive asdf)

(define text-of second)

;(*quote '(quote stuff) '()) ; stuff

; this will pass an error if called
(define initial-table
  (lambda (name)
    (car (quote ()))))

;(*identifier 'asdf '()) ; error
;(*identifier 'a '( ((1 2 3 a b c) (first second third 1 2 3)))) ; 1

;(*lambda '(lambda (a b) (cond ((eq? a b) b) (else a))) '( ((1 2 3) (a b c))))

;(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
; (non-primative ((((y z) ((8) 9)))) (x) (cons x y))

(define table-of first)
(define formals-of second)
(define body-of third)
;(third '(a b c)) ; c

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))

(define question-of first)
(define answer-of second)

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else (cons (meaning (car args) table)
		 (evlis (cdr args) table))))))

;(evlis '(cons #f 4) '()) ; ((primitive cons) #f 4)

(define function-of car)
(define arguments-of cdr)

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun) (apply-primitive (second fun) vals))
     ((non-primitive? fun) (apply-closure (second fun) vals)))))

; test functions
(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons)) (cons (first vals) (second vals)))
     ((eq? name (quote car)) (car (first vals)))
     ((eq? name (quote cdr)) (cdr (first vals)))
     ((eq? name (quote null?)) (null? (first vals)))
     ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
     ((eq? name (quote atom?)) (:atom? (first vals)))
     ((eq? name (quote zero?)) (zero? (first vals)))
     ((eq? name (quote add1)) (add1 (first vals)))
     ((eq? name (quote sub1)) (sub1 (first vals)))
     ((eq? name (quote number?)) (number? (first vals))))))

;(first '(a b))
;(apply-primitive 'null? '(())) ; #t
;(*application '(null? 2) '()) ; #f
;(*application '(cdr (quote (a b))) '()) ; (b)
;(*application '(eq? 2 (add1 1)) '()) ; #t

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure) vals)
			   (table-of closure)))))

;(value '((lambda (a b) (a (add1 b))) (lambda (c) (add1 c)) 4)) ; 6
