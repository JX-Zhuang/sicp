; 2.2.1 序列的表示
(define one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)
(cons 10 one-through-four)
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))
(list-ref one-through-four 3)
(define (length1 items)
    (if (null? items)
        0
        (+ 1 (length1 (cdr items)))))
(define (length items)
    (define (length-iter a count)
        (if (null? a)
            count
            (length-iter (cdr a) (+ count 1))))
        (length-iter items 0))
(length one-through-four)
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
(append one-through-four one-through-four)

;  练习 2.17
(define (last-pair items)
    (cond ((null? items)
            (error "items empty"))
          ((null? (cdr items))
            items)
            (else (last-pair (cdr items)))))
; (last-pair (list))
(last-pair (list 23 72 149 34))

; 练习 2.18
(define (reverse items)
    (define (reverse-iter items result)
        (if (null? items)
            result
        (reverse-iter (cdr items) (cons (car items) result))))
    (reverse-iter items (list))
)
(reverse (list 1 2 3 4))
; 练习 2.20
(define (same-parity sample . others)
    (filter (if (even? sample)
                even?
                odd?)
            (cons sample others)))
(define (filter predicate sequence)
    (cond ((null? sequence) sequence)
          ((predicate (car sequence))
            (cons (car sequence) 
                  (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter even? (list 1 2 3 4))
(filter odd? (list 1 2 3 4 5))
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 1)

; 对表对映射
; (define (map proc items)
;     (if (null? items)
;         items
;         (cons (proc (car items))
;             (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list 1 2 3 4))
; 练习 2.21
(define (square-list items)
    (if (null? items)
        ()
        (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
    (map square items))
(square-list (list 1 2 3 4))
; 练习 2.23
(define (for-each proc items)
    (if (not (null? items))
        (begin 
        (proc (car items))
        (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

; 2.2.2 层次性结构
(define x (cons (list 1 2) (list 3 4)))
(length x)
(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))
(count-leaves x)

; 练习 2.27
(define x (list (list 1 2) (list 3 4)))
(reverse x)
(define (deep-reverse tree)
    (cond ((null? tree) 
            ())
          ((not (pair? tree))
            tree)
          (else 
            (reverse (list (deep-reverse (car tree)) 
                        (deep-reverse (car (cdr tree))))))))
(deep-reverse x)

; 练习 2.28
(define x (list (list 1 2) (list 3 4)))
(define (fringe tree)
    (cond ((null? tree) 
            ())
          ((not (pair? tree))
            (list tree))
          (else 
            (append (fringe (car tree))
                    (fringe (cadr tree))))))
(fringe x)

; 练习 2.29

; 对树的映射
(define (scale-tree tree factor)
    (cond ((null? tree) ())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
    (map (lambda (sub-tree) 
        (if (pair? sub-tree)
            (scale-tree sub-tree factor)
            (* sub-tree factor)))
        tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; 练习 2.30
(define (square-list tree)
    (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-list (car tree))
                (square-list (cdr tree))))))
(square-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; 练习 2.31
(define (tree-map f tree)
    (cond ((null? tree) ())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
            (tree-map f (cdr tree))))))
(tree-map square (list (list 1 2) (list 3 4)))

; 练习 2.32

; 2.2.3 序列作为一种约定的界面

(map square (list 1 2 3 4 5))
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))

; 练习 2.33
; (define (map p sequence)
;     (accumulate (lambda (x y) 
;                     (cons (p x) y)) 
;                 () 
;     sequence))
(map square (list 1 2 3))
(define (append seq1 seq2)
    (accumulate cons seq2 seq1))
(append (list 1 2 3) (list 4 5 6))
(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3))

; 练习 2.34
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) 
                    (+ this-coeff (* x higher-terms) )) 
                    0 
                    coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

; 练习 2.35
(define (count-leaves tree)
    (accumulate + 0 (map (lambda (sub-tree) 
        (if (pair? sub-tree)
        (count-leaves sub-tree)
        1)
    ) tree)))
(count-leaves (list (list 1 2) (list 3 4)))
(count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7))))

; 练习 2.36
(define (car-n seqs)
    (map car seqs))
(define (cdr-n seqs)
    (map cdr seqs))
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        ()
        (cons (accumulate op init (car-n seqs))
              (accumulate-n op init (cdr-n seqs)))))
(define s (list (list 1 2 3)
                      (list 4 5 6)
                      (list 7 8 9)
                      (list 10 11 12)))
 (accumulate-n + 0 s)

;  练习 2.37
(define m (list (list 1 2 3 4)
                      (list 4 5 6 6)
                      (list 6 7 8 9)))
(define v (list 1 2 3 4))                    

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (col) 
            (dot-product col v))
    m))
(matrix-*-vector m v)

(define (transpose mat)
    (accumulate-n cons '() mat))
(transpose m)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (v) (matrix-*-vector cols v)) m)))
(matrix-*-matrix m (transpose m))