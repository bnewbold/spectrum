; see: https://mitpress.mit.edu/sicp/chapter1/node9.html

(define square
  (lambda (x)
    (* x x)))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- x (square guess)))
       0.001)))

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))))

(define test-sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(test-sqrt 9)
;  => 3.00009155413138
(test-sqrt (+ 100 37))
;  => 11.704699917758145
(test-sqrt (+ (test-sqrt 2) (test-sqrt 3)))
;  => 1.7739279023207892
(square (test-sqrt 1000))
;  => 1000.000369924366
