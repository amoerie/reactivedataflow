#lang racket

(require racket/date)
(require "runtime.rkt")
(include "runtime.rkt")
; Instruction types
; -----------------
(define operation make-op)
(define switch make-sw)

;; utility functions
;; Returns the first index of an element in a list or -1 if it is not found
(define (index-of xs x)
    (cond ((null? xs) -1)
          ((eq? (car xs) x) 0)
          (else (+ 1 (index-of (cdr xs) x)))))

;;
;;toegevoegd
;;
(define true #t)
(define false #f)

;; helper functions
(define (and-2 x y)  (and x y))

;;
;; see p. 37
;;
(define apply-in-underlying-scheme apply)

;;
;; see p. 6/7
;;
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
        ((tab? exp)
         (eval-tab exp env))
        ((lift? exp)
         (eval-lift (lift-operator exp) (lift-signals exp) env))
        ((application? exp)
         (apply-in-scope (eval (operator exp) env)
                         (list-of-values (operands exp) env)))

        (else
         (error "Unknown expression type -- EVAL" exp))))

;;
;; see p. 8/9
;;
(define (apply-in-scope procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((native-procedure? procedure)
         (eval-sequence
          (native-procedure-body procedure)
          (extend-environment
           (native-procedure-parameters procedure)
           arguments
           (native-procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;
;; see p. 10
;;
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;
;; see p. 11
;;
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;
;; see p. 12
;;
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;
;; see p. 13
;;
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;
;; see p. 14
;;
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;
;; see p. 15
;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;
;; see p. 16
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;;
;; see p. 17
;;
(define (variable? exp) (symbol? exp))

;;
;; see p. 18
;;
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;
;; see p. 19/20
;;
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;
;; see p. 21
;;
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;
;; see p. 22
;;
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; see p. 23/24
;;
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;
;; see p. 25
;;
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;
;; see p. 26
;;
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;
;; see p. 27
;;
(define (make-procedure parameters body env)
  (define native-procedure (list 'nativeprocedure parameters body env))
  ;; return a set of dataflow instructions which, when executed, will return the proper value
;;  (define dataflow-procedure (instructions
;;   ;; operation with index 0 : lambda that executes the procedure in our environment
;;   (operation (length parameters)
;;              (lambda args (list (apply-in-scope native-procedure args)))
;;              (ports (port (link 1 0)))
;;              )
;;  ;; operation with index 1 : lambda that displays and returns an empty result result
;;   (operation 1 (lambda (x) (display x) (newline) (list)) (ports))
;;   (ret 1)
;;  )
;; )
  ;; how to extract the return value here? 
;;  (define execution
;;    (lambda arguments
;;      (let (program-arguments (append (list dataflow-procedure 0) arguments))
;;        (
;;  (list 'dataflowprocedure lambda))
  native-procedure)

(define (native-procedure? p)
  (tagged-list? p 'nativeprocedure))

(define (native-procedure-parameters p) (cadr p))

(define (native-procedure-body p) (caddr p))

(define (native-procedure-environment p) (cadddr p))

;;
;; see p. 29
;;
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;
;; see p. 30
;;
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;;
;; see p. 31
;;
(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (frame-variables frame)))
  (set-mcdr! frame (cons val (frame-values frame))))
;;
;; see p. 32
;;
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;
;; see p. 33
;;
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;
;; see p. 34
;;
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;
;; see p. 35
;;
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'current-unix-timestamp (make-signal-wrapper current-unix-timestamp) initial-env)
    (define-variable! 'current-temp-fahrenheit (make-signal-wrapper current-temp-fahrenheit) initial-env)
    initial-env))

;;
;; see p. 36
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list 'quotient quotient)
        (list 'even? even?)
        (list 'seconds->date seconds->date)
        (list 'date->string date->string)
        (list 'string-append string-append)
        (list 'number->string number->string)
        (list 'value (lambda (exp) (signal-value (signal-wrapper-unwrap exp))))
        ;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;
;; see p. 37
;;
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define (tab? exp)
  (tagged-list? exp 'tab))

(define (tab-size exp)
  (cadr exp))

(define (tab-generator exp)
  (caddr exp))

(define (eval-tab expr env)
  (let ((size (tab-size expr))
        (generator (eval (tab-generator expr) env)))
    (define (fill-vector-loop v i)
      (if (< i size)
          (begin 
            (vector-set! v i (generator))
            (fill-vector-loop v (+ i 1)))
          v))
    (fill-vector-loop (make-vector size) 0)))
  
;; ==============================================
;; Signal-wrappers: used to flag signals in the environment
;; ==============================================
(define (make-signal-wrapper signal)
  (cons 'signal signal))

(define (signal-wrapper? exp)
  (tagged-list? exp 'signal))

(define (signal-wrapper-unwrap exp)
  (cdr exp))
  
;; ==============================================
;; Signals
;; ==============================================
;;
;; Signals: signals can derive from other signals, taking their values as input and producing something of their own.
;;          Signals without any dependencies (parents) are considered "source" signals
;;          Signals with dependencies (parents) are considered "derived" signals
;; Derived signals should be informed whenever one of their parent signals emits a new value, so that they are able to recompute their value.
;; Every signal also keeps a list of child signals, so signals can access their input-signal values (and whether or not their parents are dirty)
;; Object structure: [
;;                    value          : any,         
;;                    has-value?     : boolean
;;                    parents        : [signal],
;;                    value-provider : val1, val2, ... => value,
;;                    children       : [signal]
;;                   ]
;; The value-provider parameter is a function that takes the value from each parent signal and returns the new value for this derived signal
;;
(define (make-signal parents value-provider)
  (define signal (list->vector (list null #f parents value-provider '())))
  (for-each (lambda (parent) (signal-add-child! parent signal)) parents)
  signal)

(define (signal-value signal)
  (if (signal-has-value? signal)
      (vector-ref signal 0)
      (raise "Cannot access the value of this signal, it does not have one (yet)")))

(define (signal-value! signal value)
  (vector-set! signal 0 value)
  (signal-has-value! signal #t))

(define (signal-has-value? signal)
  (vector-ref signal 1))

(define (signal-has-value! signal has-value)
  (vector-set! signal 1 has-value))

(define (signal-parents signal)
  (vector-ref signal 2))

(define (signal-value-provider signal)
  (vector-ref signal 3))

(define (signal-children signal)
  (vector-ref signal 4))

(define (signal-children! signal children)
  (vector-set! signal 4 children))

(define (signal-add-child! signal child)
  (signal-children! signal (cons child (signal-children signal))))   

;; ==============================================
;; Built in signals
;; ==============================================

;;
;; current-unix-timestamp
;; : emits the current seconds since 1st January 1970, every second
;;
(define current-unix-timestamp (make-signal '() '(lambda (x) (res x))))
(define (current-unix-timestamp-loop callback)
  (define index (get-signal-operation-index current-unix-timestamp))
  (let loop ()
    (define new-value (current-milliseconds))
    (signal-value! current-unix-timestamp new-value)
    (callback)
    (sleep 0.00000000001)
    (loop)))

;;
;; current-temp-fahrenheit
;; : emits a random number between 1 and 100, every (random 1 .. 10 seconds)
;;
(define current-temp-fahrenheit (make-signal '() '(lambda (x) (res x))))
(define (current-temp-fahrenheit-loop callback)
  (define index (get-signal-operation-index current-temp-fahrenheit))
  (let loop ()
    (sleep (random 1 10))
    (define new-value (random 1 100))
    (signal-value! current-temp-fahrenheit new-value)
    (callback)
    (loop)))

;; ==============================================
;; Signal graph + update loop
;; ==============================================
(define source-signals (list current-unix-timestamp))

(define (get-topologically-sorted-signals)
  (define (topological-sort accumulator next-children)
    (if (null? next-children)
        accumulator
        (topological-sort (append accumulator (filter (lambda (child) (not (member child accumulator))) next-children))
                          (foldl append '() (map signal-children next-children)))))
  (topological-sort '() source-signals))

;; ==============================================
;; Lifting
;; One or more signals can be lifted with a procedure to create a new signal
;; Syntax: (lift (lambda (value1 value2 ...) ( ... )) signal1 signal2 ...)
;; ==============================================
(define (lift? exp) (tagged-list? exp 'lift))
(define (lift-operator exp) (cadr exp))
(define (lift-signals exp) (cddr exp))

(define (eval-lift operator-exp signal-exps env)
  (define wrapped-parents (map (lambda (signal-exp) (eval signal-exp env)) signal-exps))
  (define parents (map signal-wrapper-unwrap wrapped-parents))
  (make-signal-wrapper (make-signal parents operator-exp)))
  
(define (print-input input)
  (newline)
  (display input))

(define (print-output output)
  (newline)
  (if (native-procedure? output)
      (display (list 'native-procedure
                     (native-procedure-parameters output)
                     (native-procedure-body output)
                     '<native-procedure-env>))
      (display output))
  (newline))

(define (print-result result)
  (print-input (car result))
  (print-output (cdr result)))

(define the-global-environment (setup-environment))

(define (evaluate input)
  (define output (eval input the-global-environment))
  output)

;; ====================================
;;        INITIAL PROGRAM INPUTS
;;
;; This evaluates any instructions before setting up the signals in the dataflow system
;; After these initial inputs, no new signals can be added 
;; ====================================

(define program-inputs
  (list
   ;;'(define x 18)
   ;;'x
   ;;'(define (test a b c) (+ a b c))
   ;;'(test 1 2 3)
   ;;'(define (fahrenheit->celsius fahrenheit)
   ;;  (quotient (* (- fahrenheit 32) 5) 9))
   ;;'(define current-temp-celsius
   ;;   (lift fahrenheit->celsius current-temp-fahrenheit))
   ;;'(define current-date
   ;;   (lift seconds->date current-unix-timestamp))
   ;;'(define billboard-label
   ;;    (lift
   ;;     (lambda (temperature date)
   ;;       (string-append "Temperature: " (number->string temperature) "C°, Date: " (date->string date)))
   ;;     current-temp-celsius
   ;;     current-date))   
   '(define fanout-signal-0 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-1 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-2 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-3 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-4 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-5 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-6 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-7 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-8 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-9 (lift (lambda (x) (res x))  current-unix-timestamp))
   '(define fanout-signal-10 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-11 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-12 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-13 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-14 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-15 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-16 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-17 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-18 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-19 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-20 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-21 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-22 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-23 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-24 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-25 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-26 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-27 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-28 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-29 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-30 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-31 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-32 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-33 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-34 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-35 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-36 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-37 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-38 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-39 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-40 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-41 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-42 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-43 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-44 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-45 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-46 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-47 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-48 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-49 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-50 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-51 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-52 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-53 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-54 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-55 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-56 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-57 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-58 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-59 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-60 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-61 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-62 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-63 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-64 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-65 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-66 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-67 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-68 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-69 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-70 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-71 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-72 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-73 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-74 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-75 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-76 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-77 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-78 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-79 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-80 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-81 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-82 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-83 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-84 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-85 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-86 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-87 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-88 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-89 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-90 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-91 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-92 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-93 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-94 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-95 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-96 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-97 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-98 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-99 (lift (lambda (x) (res x)) current-unix-timestamp))
   '(define fanout-signal-100 (lift 
                               (lambda (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49 v50 v51 v52 v53 v54 v55 v56 v57 v58 v59 v60 v61 v62 v63 v64 v65 v66 v67 v68 v69 v70 v71 v72 v73 v74 v75 v76 v77 v78 v79 v80 v81 v82 v83 v84 v85 v86 v87 v88 v89 v90 v91 v92 v93 v94 v95 v96 v97 v98 v99)
                                 (res v99)))
      fanout-signal-0 fanout-signal-1 fanout-signal-2 fanout-signal-3 fanout-signal-4 fanout-signal-5 fanout-signal-6 fanout-signal-7 fanout-signal-8 fanout-signal-9 fanout-signal-10 fanout-signal-11 fanout-signal-12 fanout-signal-13 fanout-signal-14 fanout-signal-15 fanout-signal-16 fanout-signal-17 fanout-signal-18 fanout-signal-19 fanout-signal-20 fanout-signal-21 fanout-signal-22 fanout-signal-23 fanout-signal-24 fanout-signal-25 fanout-signal-26 fanout-signal-27 fanout-signal-28 fanout-signal-29 fanout-signal-30 fanout-signal-31 fanout-signal-32 fanout-signal-33 fanout-signal-34 fanout-signal-35 fanout-signal-36 fanout-signal-37 fanout-signal-38 fanout-signal-39 fanout-signal-40 fanout-signal-41 fanout-signal-42 fanout-signal-43 fanout-signal-44 fanout-signal-45 fanout-signal-46 fanout-signal-47 fanout-signal-48 fanout-signal-49 fanout-signal-50 fanout-signal-51 fanout-signal-52 fanout-signal-53 fanout-signal-54 fanout-signal-55 fanout-signal-56 fanout-signal-57 fanout-signal-58 fanout-signal-59 fanout-signal-60 fanout-signal-61 fanout-signal-62 fanout-signal-63 fanout-signal-64 fanout-signal-65 fanout-signal-66 fanout-signal-67 fanout-signal-68 fanout-signal-69 fanout-signal-70 fanout-signal-71 fanout-signal-72 fanout-signal-73 fanout-signal-74 fanout-signal-75 fanout-signal-76 fanout-signal-77 fanout-signal-78 fanout-signal-79 fanout-signal-80 fanout-signal-81 fanout-signal-82 fanout-signal-83 fanout-signal-84 fanout-signal-85 fanout-signal-86 fanout-signal-87 fanout-signal-88 fanout-signal-89 fanout-signal-90 fanout-signal-91 fanout-signal-92 fanout-signal-93 fanout-signal-94 fanout-signal-95 fanout-signal-96 fanout-signal-97 fanout-signal-98 fanout-signal-99
      )

  )
)
(for-each evaluate program-inputs)

;; ====================================
;;        DATAFLOW INSTRUCTIONS
;;
;; Once all signals are known, they can be converted into dataflow instructions
;; Every instruction represents one signal
;; ====================================
;; The flat list of topologically sorted signals
(define topologically-sorted-signals (get-topologically-sorted-signals))

;; Returns a unique index per signal that identifies the associated dataflow operation
(define (get-signal-operation-index signal)
  (+ (index-of topologically-sorted-signals signal) 1))

;; Returns the argument index that the provided signal should use to send its new value to the provided child (this will be used for the port of the signal operation)
(define (get-signal-operation-argument-index signal child)
  (index-of (signal-parents child) signal))

;; (operation number-of-inputs lambda ports)
;;
;; The lambda takes n arguments, where n = number-of-inputs, and sends the output to each port defined in the ports

;; (ports port1 port2 port3 ...)
;; (port link)
;; 
;; link = a reference to an argument of another instruction

;; (link address port-number)
;;
;; address = index of the instruction, port-number = argument-index of the instruction
(define (get-make-ro-val-operation-index)
  (+ 1 (length topologically-sorted-signals))) 

;; For each signal:
;; operation with index i : lambda that takes p arguments where p = # parents
(define (make-signal-operation signal)
  (define parents (signal-parents signal))
  (define children (signal-children signal))
  (define value-provider (signal-value-provider signal))
  (define (make-signal-operation-output-link child)
    (quasiquote (link (unquote (get-signal-operation-index child)) (unquote (get-signal-operation-argument-index signal child)))))

  ;; the number of arguments of the dataflow operation is equal to the number of parent signals.
  ;; Source signals technically have no parents, so their value provider is just the identity function (lambda (x) x)
  ;; This does mean that while they don't have any parents, their operation-number-of-args has to be 1
  (define operation-number-of-args
    (cond ((eq? (member signal source-signals) #f) (length parents))
          (else 1)))

  ;; executing a signal means computing the new value using the value-provider and passing the new value to the children  
  (define operation-lambda value-provider)

  ;; for each child (dependent signal) we create a link which identifies the operation index and argument index
  ;; If a certain signal has no children, it is an output signal and we sent its values to the make-ro-val instruction
  (define operation-ports
    (cond ((null? children) (quasiquote (ports (port (link (unquote (get-make-ro-val-operation-index)) 0)))))
          (else (quasiquote (ports (port (unquote-splicing (map make-signal-operation-output-link children))))))))
  (quasiquote (operation (unquote operation-number-of-args) (unquote operation-lambda) (unquote operation-ports))))

(define (make-instructions)
  (define make-ro-store-instruction (list '(make-ro-store)))
  (define signal-operations (map make-signal-operation topologically-sorted-signals))
  (define make-ro-val-instruction (list '(make-ro 'val)))
  (define operations (append make-ro-store-instruction signal-operations make-ro-val-instruction))
  (apply instructions operations))

;; Creates 1 new dataflow context + input tokens for each source signal of their current values and pushes those into the token queue
(define (push-current-unix-timestamp dataflow-manager)
  (add-inputs-with-return! dataflow-manager 1 (list (signal-value current-unix-timestamp))))

;; Infinitely tries to process tokens in the dataflow runtime
;; Sleeps a few ms between every loop
;; Skips processing when there are no tokens to process
(define (dataflow-manager-processor-loop dataflow-manager timeout)
  (define results '())
  (let loop ()
    (define timestamp (current-milliseconds))
    (if (< timestamp timeout)
      (let ((runtime-result (get-value! dataflow-manager)))
        (set! results (cons (string-append (number->string timestamp) ";" (number->string (car runtime-result)) " ") results))
        (sleep 0.0000001)
        (loop))
      (for-each display results))))

(define (startup-dataflow-runtime)
  (newline) (display "Creating dataflow instructions")
  (define dataflow-instructions (make-instructions))
  (newline) (display "OK Created dataflow instructions")

  (define dataflow-manager (start-runtimes dataflow-instructions 1))
  (define source-signal-callback (lambda () (push-current-unix-timestamp dataflow-manager)))
  
  (newline) (display "Starting up current-unix-timestamp-loop")
  (define t1 (thread (lambda () (current-unix-timestamp-loop source-signal-callback))))
  ;;(newline) (display "Starting up current-temp-fahrenheit-loop")
  ;;(define t2 (thread (lambda () (current-temp-fahrenheit-loop source-signal-callback))))
  (newline) (display "Starting up dataflow-manager-processor-loop")
  (define t3 (thread (lambda () (dataflow-manager-processor-loop dataflow-manager (+ (current-milliseconds) 6000)))))
  ;;(dataflow-manager-processor-loop dataflow-manager)
  (sleep 10)
  (newline) (display "Shutting down")
  (kill-thread t1)
  ;;(kill-thread t2)
  ;;(kill-thread t3)
  )

(startup-dataflow-runtime)

;; ====================================
;;                REPL
;; ====================================
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (print-output output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;;(driver-loop)