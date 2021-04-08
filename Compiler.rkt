#lang racket

(require rackunit)

;; Translates a lambda calculus program to a ES6 Javascript program
(define (translate program)
  (match program
    [(? number?) program]
    [(? symbol?) program]
    [(list `println LC) (list 'console.log (list (translate LC)))]
    [(list '/ param '=> body) (list param '=> (translate body))]
    [(list func arg) (list (translate func) (list (translate arg)))]
    [(list '+ left right) (list (translate left) '+ (translate right))]
    [(list '* left right) (list (translate left) '* (translate right))]
    [(list 'ifleq0 guard then else)
     (list
      (list (translate guard) '<= 0) `? (translate then) ': (translate else))]))

(check-equal? (translate '5) 5)
(check-equal? (translate 'x) 'x)
(check-equal? (translate '(/ x => (+ x 14))) '(x => (x + 14)))
(check-equal? (translate '(+ 1 2)) '(1 + 2))
(check-equal? (translate '(* 2 3)) '(2 * 3))
(check-equal? (translate '(+ (+ 1 2) (+ (+ 3 4) (+ 5 6)))) '((1 + 2) + ((3 + 4) + (5 + 6))))
(check-equal? (translate '(ifleq0 1 2 3)) '((1 <= 0) ? 2 : 3))
(check-equal? (translate '(println 1)) '(console.log(1)))
(check-equal? (translate '(println ((/ x => (+ x 14)) (+ 1 5)))) '(console.log(((x => (x + 14)) ((1 + 5))))))
(check-equal? (translate '(println (ifleq0 ((/ x => (+ x -5)) 1) (* 3 3) (+ 3 3))))
              '(console.log(((((x => (x + -5)) (1)) <= 0) ? (3 * 3) : (3 + 3)))))