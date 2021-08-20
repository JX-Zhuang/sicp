; 3.1.1 局部状态变量

; 练习 3.1
(define (make-accumulator value)
    (lambda (add-value)
        (set! value (+ value add-value))
        value))
 (define A (make-accumulator 5))
 (A 10)
 (A 5)