
; cyclic dependency graph (sigh)
;
; meaning
;   expression-to-action
;     list-to-action
;       *application
;         meaning
;
;   actions

; ### preliminaries, utilities, shorthand

; check if something is an atom vs {null, collection}
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; need a list or tuple type; tuples prefered
(define first
  (lambda (p) (car p)))

(define second 
  (lambda (p) (car (cdr p))))

(define third
  (lambda (p) (car (cdr (cdr p)))))

(define build
  (lambda (a b) (cons a (cons b (quote ())))))

(define text-of second)

; test functions
(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

; table operations
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

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name 
			    (car table)
			    (lambda (n)
			      (lookup-in-table n (cdr table) table-f)))))))

(define initial-table
  (lambda (name)
    (car (quote ()))))

;(lookup-in-entry 'fish
;		 '((teach a man to fish)
;		   (1 2 3 4 5))
;		 (lambda (x) x))

;(lookup-in-table 'fish
;		 (extend-table '((teach a man to fish)
;				 (1 2 3 4 5))
;			       (quote ()))
;		 (lambda (x) x))

; ### specific types/helpers
(define builtin?
  (lambda (l)
    (eq? (first l) (quote builtin))))

(define non-builtin?
  (lambda (l)
    (eq? (first l) (quote non-builtin))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))

(define table-of first)
(define formals-of second)
(define body-of third)
(define question-of first)
(define answer-of second)
(define cond-lines-of cdr)
(define function-of car)
(define arguments-of cdr)

; need generic true/false booleans, a number type, and a symbol type
; also need a mutable "table" collection
(define *const
  (lambda (e table) 
    (cond          
     ((number? e) e) 
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote builtin) e)))))
;(*const 'asdf '()) ; (builtin asdf)

(define *lambda
  (lambda (e table)
    (build (quote non-builtin) (cons table (cdr e)))))
;(*lambda '(lambda (a b) (cond ((eq? a b) b) (else a))) '( ((1 2 3) (a b c))))
;  (non-builtin ((((1 2 3) (a b c))) (a b) (cond ((eq? a b) b) (else a))))

(define *quote
  (lambda (e table)
    (text-of e)))
;(*quote '(quote stuff) '()) ; stuff

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
;(*identifier 'asdf '()) ; error
;(*identifier 'a '( ((1 2 3 a b c) (first second third 1 2 3)))) ; 1

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote builtin)) #t)
     ((eq? (car x) (quote non-builtin)) #t)
     (else #f))))

; ### now we start the meat!

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

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else (cons (meaning (car args) table)
		 (evlis (cdr args) table))))))
;(evlis '(cons #f 4) '()) ; ((builtin cons) #f 4)

(define *application
  (lambda (e table)
    (apply2
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

; basic, low-level, non-compound functions
(define apply-builtin
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

; for compound functions
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure) vals)
			   (table-of closure)))))

; this is "how apply would be implemented"; it isn't used in this file
(define apply2
  (lambda (fun vals)
    (cond
     ((builtin? fun) (apply-builtin (second fun) vals))
     ((non-builtin? fun) (apply-closure (second fun) vals)))))

; find the value of an s-expression in the context of an environment
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
; (non-primative ((((y z) ((8) 9)))) (x) (cons x y))

; and finally, helper to find values in a starting environment
(define value
  (lambda (e)
    (meaning e (quote ()))))
;(value '((lambda (a b) (a (add1 b))) (lambda (c) (add1 c)) 4)) ; 6
