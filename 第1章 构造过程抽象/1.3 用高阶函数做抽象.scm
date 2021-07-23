(define (cube x)
    (* x x x))
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
            (sum term (next a) next b))))
(define (inc n)
    (+ n 1))
(define (sum-cubes a b)
    (sum cube a inc b))
(sum-cubes 1 10)
(define (identity x)
    x)
; (define (sum-integers a b)
;     (sum identity a inc b))
; (sum-integers 1 10)
(define (pi-sum a b)
    (define (pi-iterm x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-iterm a pi-next b))
(* 8 (pi-sum 1 1000))
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; 1.30
(define (sum1 term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
        (iter a 0))
(define (sum-integers a b)
(sum1 identity a inc b))
(sum-integers 1 10) 

; 1.31
(define (product1 term a next b)
    (if (> a b)
        1
        (* (term a)
            (product1 term (next a) next b))))
(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
        (iter a 1))
(product (lambda (x) x) 1 (lambda (i) (+ i 1)) 10)
(define (numer-term i)
    (cond ((= i 1)
        2)
        ((even? i)
        (+ i 2))
        (else
        (+ i 1))))
(define (denom-term i)
    (if (odd? i)
        (+ i 2)
        (+ i 1)))
(define (pi n)
    (* 4 
        (exact->inexact
            (/ 
                (product numer-term 
                    1 
                    (lambda (x) (+ x 1)) 
                    n)
                (product denom-term
                    1 
                    (lambda (x) (+ x 1)) 
                    n)
            )
        )
    )
)
(pi 1)
(pi 10)
(pi 100)
(pi 1000)
(pi 10000)
; 1.34
(define (f g)
    (g 2))
; (f f)
; 1.37
(define (cont-frac1 N D k)
    (define (cf i)
        (if (= i k)
            (/ (N i) (D i))
            (/ (N i)
                (+ (D i) (cf (+ i 1))))))
    (cf 1))

(define (cont-frac N D k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (N i) (+ (D i) result)))))
    (iter (- k 1) (/ (N k) (D k))))

(define (golden-ratio k)
    (+ 1
       (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))
(golden-ratio 1)
(golden-ratio 10)
(golden-ratio 11)
; 1.39
(define (tan-cf x k)
    (define (N i)
        (if (= i 1) 
            x
            (- (square x))))
    (define (D i)
        (- (* i 2) 1))
   (exact->inexact (cont-frac N D k)))
(tan 10)
(tan-cf 10 100)
(tan 25)
(tan-cf 25 100)

(define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))
((average-damp square) 10)