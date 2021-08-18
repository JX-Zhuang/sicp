; 2.4.1 复数的表示
(define (add-complex z1  z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (real-part z2))))
(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))                        
(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))  
; 直角坐标系
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
    (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
; 极坐标形式
(define (real-part z)
    (* (magnitude z) (cos (angle z))))
(define (imag-part z)
    (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
(define (make-form-mag-ang r a) (cons r a))

; 2.4.2 带标志数据
(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "error--type-tag") datum))
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "error--contents") datum))
(define (rectangular? z)
    (eq? (type-tag z) `rectangular))
(define (polar? z)
    (eq? (type-tag z) `polar))

; 直角坐标系
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
    (sqrt (+ (square (real-part-rectangular z)) 
             (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
    (atan (imag-part-rectangular z) 
          (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) 
    (attach-tag `rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
    (attach-tag `rectangular 
                (cons (* r (cos a)) (* r (sin a)))))

; 极坐标形式
(define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
    (attach-tag `polar 
                (cons (sqrt (+ (square x) (square y)))
                      (atan y x))))
(define (make-form-mag-ang-polar r a) 
    (attach-tag `polar (cons r a)))

(define (real-part z)
    (cond ((rectangular? z)
            (real-part-rectangular (contents z)))
          ((polar? z)
            (real-part-polar (contents z)))
          (else (error "unknow type real-part" z))))
(define (imag-part z)
    (cond ((rectangular? z)
            (imag-part-rectangular (contents z)))
          ((polar? z)
            (imag-part-polar (contents z)))
          (else (error "unknow type imag-part" z))))
(define (magnitude z)
    (cond ((rectangular? z)
            (magnitude-rectangular (contents z)))
          ((polar? z)
            (magnitude-polar (contents z)))
          (else (error "unknow type magnitude" z))))
(define (angle z)
    (cond ((rectangular? z)
            (angle-rectangular (contents z)))
          ((polar? z)
            (angle-polar (contents z)))
          (else (error "unknow type angle" z))))
(define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
    (make-from-mag-ang-polar r a))

; 2.4.3 数据导向的程序设计和可加性
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; 消息传递
(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond ((eq? op `real-part) x)
              ((eq? op `imag-part) y)
              ((eq? op `magnitude)
                (sqrt (+ (square x) (square y))))
              ((eq? op `angle) (atan y x))
              (else
                (error "Make-from-real-imag error" op))))
    dispatch)

; 2.5 带有通用型操作的系统

; 2.5.1 通用型算术运算

(define (add x y) (apply-generic `add x y))
(define (sub x y) (apply-generic `sub x y)) 
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))

; 2.5.2 不同类型数据的组合

; 类型的层次结构