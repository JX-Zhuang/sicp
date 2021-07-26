; 2.1.1 有理数第算术运算
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) 
                 (* (numer y) (denom x))) 
              (* (denom x) (denom y))))
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) 
                 (* (numer y) (denom x))) 
              (* (denom x) (denom y))))
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) 
              (* (denom x) (denom y))))
(define (div-rat x y)
    (make-rat (* (numer x) (denom y)) 
              (* (denom x) (numer y))))
(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
(define x (cons 1 2))
(car x)
(cdr x)
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
(define (make-rat n d)
    (let ((g (gcd n d)))
     (cons (/ n g) (/ d g))))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
; 练习 2.1
(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)))
; 2.1.2 抽象屏障
; 练习 2.2
