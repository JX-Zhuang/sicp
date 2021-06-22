; 基本表达形式：用于表示语言所关心的最简单的个体
; 组合的方法：通过它们可以从较简单的东西触发构造出复合的元素
; 抽象的方法：通过它们可以为复合对象命名，并将它们当作单元去操作
(define (square x)
    (* x x))
(define (sum-of-squares x y)
    (+ (square x) (square y)))
(sum-of-squares 3 4)
(define (abs1 x)
    (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (abs x)
    (if (< x 0)
        (- x)
        x))
(abs 10)
(abs 0)
(abs (- 10))
;;EXERCISE 1.1
(define a 3)        ;; 3
(define b (+ a 1))  ;; 4
(+ a b (* a b))     ;; 19
(= a b)             ;;
(if (and (> b a) (< b (* a b)))
    b
    a)      ;;4
(cond ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25))
(+ 2 (if (> b a) b a))  ;;6
(* (cond ((> a b) a)
    ((< a b) b)
    (else -1))
    (+ a 1))    ;;16
(define (max a b)
    (if (> a b) a b))
(max 1 2)
(max 2 1)
(max 1 1)
(define (max-three a b c)
    (max (max a b) (max b c)))
(max-three 1 2 3)
(max-three 3 2 1)
(max-three 1 3 2)
(max-three 1 3 1)
(define (p) (p))
(define (test x y)
    (if (= x 0)
    0
    y))
; (test 0 (p))
; 正则序：完全展开而后规约
; 应用序：先求值参数而后应用
; 1.1.7
; (define (sqrt-iter guess x)
;     (if (good-enough? guess x)
;     guess
;     (sqrt-iter (improve guess x)
;     x)))
; (define (improve guess x)
;     (average guess (/ x guess)))
; (define (average x y)
;     (/ (+ x y) 2))
; (define (good-enough? guess x)
;     (< (abs (- (square guess) x)) 0.001))
; (define (sqrt x)
;     (sqrt-iter 1.0 x))



(define (sqrt-iter guess x)
    (if(good-enough guess x)
        guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x)
    (average guess (/ x guess)))
(define (good-enough guess x)
    (< (abs (- (square guess) x)) 0.001))
(define (average x y)
    (/ (+ x y) 2))
(define (sqrt x)
    (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))