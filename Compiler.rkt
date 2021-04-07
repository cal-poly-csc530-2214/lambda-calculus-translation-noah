#lang racket

(require rackunit)

(define (translate program)
  (match program
    [(? number?) program]
    [(? symbol?) program]
    [(list '/ param '=> body) (list param '=> (translate body))]
    [(list '+ left right) (list (translate left) '+ (translate right))]
    [(list '* left right) (list (translate left) '* (translate right))]
    [(list 'ifleq0 guard then else)
     (list
      (list (translate guard) '<= 0) `? (translate then) ': (translate else))]
    [(list `println LC) (list 'console.log\( (translate LC) '\) )]))

(check-equal? (translate '5) 5)
(check-equal? (translate 'x) 'x)
(check-equal? (translate '(/ x => (+ x 14))) '(x => (x + 14)))
(check-equal? (translate '(+ 1 2)) '(1 + 2))
(check-equal? (translate '(* 2 3)) '(2 * 3))
(check-equal? (translate '(+ (+ 1 2) (+ (+ 3 4) (+ 5 6)))) '((1 + 2) + ((3 + 4) + (5 + 6))))
(check-equal? (translate '(ifleq0 1 2 3)) '((1 <= 0) ? 2 : 3))
(check-equal? (translate '(println 1)) '(console.log\( 1 \)))



