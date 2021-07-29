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
