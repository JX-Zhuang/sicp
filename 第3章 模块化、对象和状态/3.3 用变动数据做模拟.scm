; 3.3.1 变动的表结构
; 3.3.2 队列的表示
; 3.3.3 表格的表示
; 3.3.4 数字电路的模拟器
; 3.3.5 约束的传播
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        `ok))
(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2))
               (set-value! sum
                           (+ (get-value a1) (get-value a2))
                           me))
              ((and (has-value? a1) (has-value? sum))
               (set-value! a2
                           (- (get-value sum) (get-value a1))
                           me))
              ((and (has-value? a2) (has-value? sum))
               (set-value! a1
                           (- (get-value sum) (get-value a2))
                           me))))
    (define (process-forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
        (cond ((eq? request `I-have-a-value)
               (process-new-value))
              ((eq? request `I-lost-my-value)
               (process-forget-value))
              (else
                (error "ADDER REQUEST ERROR" request))))
    (conncet a1 me)
    (conncet a1 me)
    (conncet sum me)
    me)

(define (inform-about-value constraint)
    (constraint `I-have-a-value))
(define (inform-about-no-value constraint)
    (constraint `I-lost-my-value))

