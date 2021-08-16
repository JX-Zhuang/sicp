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
; (define (equal? x y)
;     (cond ((and (symbol? x) (symbol? y))
;         (symbol-eq? x y))
;         ((and (list? x) (list? y))
;         (list-equal? x y))
;         (else
;             (error "error--" x y))))
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

; 练习 2.56

; 2.3.3 实例：集合的表示

; 集合作为未排序的表
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
; 交集
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
            (cons (car set1)
                  (intersection-set (cdr set1) set2)))
            (else (intersection-set (cdr set1) set2))))

; 练习 2.59 
; 并集
(define (union-set set1 set2)
    (iter (append set1 set2) `()))

(define (iter input result)
    (if (null? input)
        (reverse result)
        (let ((current-element (car input))
              (remain-element (cdr input)))
            (if (element-of-set? current-element result)
                (iter remain-element result)
                (iter remain-element
                      (cons current-element result))))))
(union-set `(1 2 3) `(3 4 5 6))

; 集合作为排序的表
(define (element-of-set? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((<x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        `()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (intersection-set (cdr set1)
                                           (cdr set2))))
                  ((< x1 x2)
                    (intersection-set (cdr set1) set2))
                  ((< x2 x1)
                    (intersection-set set1 (cdr set2)))))))

; 练习 2.61
; 练习 2.62
; 集合作为二叉树
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))
(define (element-of-set? x set)
    (cond ((null? set) false)
    ((= x (entry set)) true)
     ((< x (entry set))
      (element-of-set? x (left-branch set)))
     ((> x (entry set))
      (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x `() `()))
          ((= x (entry set)) set)
          ((< x (entry set))
            (make-tree (entry set)
                       (adjoin-set x (left-branch set))
                       (right-branch set)))
          ((> x (entry set))
            (make-tree (entry set)
                       (left-branch set)
                       (adjoin-set x (right-branch set))))))

; 练习 2.63
; 练习 2.64
; 练习 2.65
; 集合与信息检索
; 练习 2.66

; 2.3.4 实例：Huffman编码树
(define (make-leaf symbol weight)
    (list `leaf symbol weight))
(define (leaf? object)
    (eq? (car object) `leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (caddr tree)))