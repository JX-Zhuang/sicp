; 2.3.1 引号
(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))
(memq `apple `(pear banana prune))
(memq `apple `(x (apple sauce) y apple pear))
; 练习 2.53
(list `a `b `c)
(memq `red `(red shoes blue))
(car `(red shoes blue))

; 练习 2.54
(define (equal? x y)
    (cond ((and (symbol? x) (symbol? y))
        (symbol-eq? x y))
        ((and (list? x) (list? y))
        (list-equal? x y))
        (else
            (error "error--" x y))))
(define (symbol-eq? x y)
    (eq? x y))
(define (list-equal? x y)
    (cond ((and (null? x) (null? y))
        #t)
          ((or (null? x) (null? y))
        #f)
        ((equal? (car x) (car y))
         (equal? (cdr y) (cdr y)))
        (else 
         #f)))
(list? (list 1 2 3))
(equal? 'symbol 'symbol)
(equal? 'symbol 'another-symbol)
(equal? (list 'a 'b 'c) (list 'a 'b 'c))
(equal? (list 'a) (list 'a 'b 'c))

; 练习 2.55
(car ''abracadabra)

; 2.3.2 实例：符号求导

; 对抽象数据对求导程序
(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else 
            (error "deriv error" exp))))

; 代数表达式的表示
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list `+ a1 a2))))
(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list `* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) `+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) `*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(deriv `(+ x 3) `x)
(deriv `(* x y) `x)
(deriv `(* (* x y) (+ x 3)) `x)