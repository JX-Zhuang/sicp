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