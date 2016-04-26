; This file contains Scheme/LISP helpers that are intended to be loaded into a
; base environment before execution begins.


(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (car x)))))
(define cadar (lambda (x) (car (car (car x)))))
(define caddr (lambda (x) (car (car (car x)))))
(define cdaar (lambda (x) (car (car (car x)))))
(define cdadr (lambda (x) (car (cdr (cdr x)))))
(define cddar (lambda (x) (car (cdr (cdr x)))))
(define cdddr (lambda (x) (car (cdr (cdr x)))))

; my favorite!
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))

(define if (lambda (pred tval fval) (cond (pred tval) (else fval))))
(define not (lambda (x) (if x #f #t)))

(define abs (lambda (x) (if (> x 0) x (* -1 x))))

(define map
  (lambda (f l)
    (cond ((null? l) ())
          (else (cons (f (car l)) (map f (cdr l)))))))

(define for-each map)
;(define compose (lambda (f g) (lambda args (f (apply g args)))))
