(load "breadth.scm")

(define (jarra5 x) (car x))

(define (jarra8 x) (car (cdr x)))

(define bc '(
       (lambda (x) (if (< (jarra5 x) 5) (list 5 (jarra8 x)) x))
       (lambda (x) (if (> (jarra5 x) 0) (list 0 (jarra8 x)) x))
       (lambda (x) (if (>= (- 5 (jarra5 x)) (jarra8 x)) (list (+ (jarra5 x) (jarra8 x)) 0) x))
       (lambda (x) (if (< (- 5 (jarra5 x)) (jarra8 x)) (list 5 (- (jarra8 x) (- 5 (jarra5 x)))) x))
       (lambda (x) (if (< (jarra8 x) 8) (list (jarra5 x) 8) x))
       (lambda (x) (if (> (jarra8 x) 0) (list (jarra5 x) 0) x))
       (lambda (x) (if (>= (- 8 (jarra8 x)) (jarra5 x)) (list 0 (+ (jarra8 x) (jarra5 x))) x))
       (lambda (x) (if (< (- 8 (jarra8 x)) (jarra5 x)) (list (- (jarra5 x) (- 8 (jarra8 x))) 8) x))
    )
)
