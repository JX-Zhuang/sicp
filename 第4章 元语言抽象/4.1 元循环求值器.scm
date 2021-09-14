; 4.1.1 求值器的内核
(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else 
            (error "eval error:" exp))))

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-parameters procedure)
                    arguments
                    (procedure-environment procedure))))
            (else
                (error "apply error:" procedure))))
; 过程参数
(define (list-of-values exps env)
    (if (no-operands? exps)
        `()
        (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) evn))))
; 条件
(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
; 序列
(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
; 赋值和定义
(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    `ok)
(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                     (eval (definition-value exp) env)
                     env)
    `ok)

; 4.1.2 表达式的表示
; 4.1.3 求值器数据结构
(define (true? x)
    (not (eq? x false)))
(define (false x)
    (eq? x false))
; 4.1.4 作为程序运行这个求值器
; 4.1.5 将数据作为程序
; 4.1.6 内部定义