#lang racket/base

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
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
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
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

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
    (define-variable! '$current-seconds (make-signal-wrapper $current-seconds) initial-env)
    ;;(define-variable! '$random-integer (make-signal-wrapper $random-integer) initial-env)
    initial-env))

;;
;; see p. 36
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(define strings '())

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list 'string-append string-append)
        (list 'number->string number->string)
        (list 'current-milliseconds current-milliseconds)
        (list 'display display)
        (list 'newline newline)
        (list 'even? even?)
        (list 'value (lambda (exp) (signal-value (signal-wrapper-unwrap exp))))
        (list 'save-string (lambda (str) (set! strings (cons str strings))))
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
(define (make-signal-wrapper $signal)
  (cons 'signal $signal))

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
;;                    up-to-date?    : boolean,
;;                    parents        : [signal],
;;                    value-provider : val1, val2, ... => value,
;;                    children       : [signal]
;;                   ]
;; The value-provider parameter is a function that takes the value from each parent signal and returns the new value for this derived signal
;;
(define (make-signal parents value-provider)
  (define $signal (list->vector (list null #f #f parents value-provider '())))
  (for-each (lambda ($parent) (signal-add-child! $parent $signal)) parents)
  $signal)

(define (signal-value $signal)
  (if (signal-has-value? $signal)
      (vector-ref $signal 0)
      (raise "Cannot access the value of this signal, it does not have one (yet)")))

(define (signal-value! $signal value)
  (vector-set! $signal 0 value)
  (signal-has-value! $signal #t))

(define (signal-has-value? $signal)
  (vector-ref $signal 1))

(define (signal-has-value! $signal has-value)
  (vector-set! $signal 1 has-value))

(define (signal-up-to-date? $signal)
   (vector-ref $signal 2))

(define (signal-up-to-date! $signal up-to-date)
  (vector-set! $signal 2 up-to-date))

(define (signal-parents $signal)
  (vector-ref $signal 3))

(define (signal-value-provider $signal)
  (vector-ref $signal 4))

(define (signal-children $signal)
  (vector-ref $signal 5))

(define (signal-children! $signal children)
  (vector-set! $signal 5 children))

(define (signal-add-child! $signal $child)
  (signal-children! $signal (cons $child (signal-children $signal))))

;;
;; Recomputes the value of the provided signal, provided all parents have values
;; When the value is set, it flags the signal as being up to date
;; and flags every child as being NOT up to date
;;
(define (signal-update! $signal)
  (define parents (signal-parents $signal))
  (define children (signal-children $signal))
  (define parents-have-values (map signal-has-value? parents))
  (define all-parents-have-values? (foldl and-2 #t parents-have-values))
  (if all-parents-have-values?
      (let ((value-provider (signal-value-provider $signal))
            (parent-values (map signal-value parents)))
           (signal-value! $signal (apply value-provider parent-values))
           (signal-up-to-date! $signal #t)
           (for-each (lambda ($child) (signal-up-to-date! $child #f)) children))
      #f))
      

;; ==============================================
;; Built in signals
;; ==============================================

;;
;; $current-seconds
;; : emits the current seconds since 1st January 1970, every second
;;
(define $current-seconds (make-signal '() current-milliseconds))
(define (current-seconds-loop)
  (signal-up-to-date! $current-seconds #f)
  (sleep 0.1)
  (current-seconds-loop))

;;
;; $random
;; : emits a random number between 1 and 100, every (random 1 .. 10 seconds)
;;
;;(define $random-integer (make-signal '() (lambda () (random 1 100))))
;;(define (random-integer-loop)
;;  (signal-up-to-date! $random-integer #f)
;; (sleep 0.01)
;;  (random-integer-loop))

;; ==============================================
;; Signal graph + update loop
;; ==============================================
(define source-signals (list $current-seconds))

(define (get-topologically-sorted-signals)
  (define (topological-sort accumulator next-children)
    (if (null? next-children)
        accumulator
        (topological-sort (append accumulator next-children) (foldl append '() (map signal-children next-children)))))
  (topological-sort '() source-signals))

(define (update-signals-loop)
  (define signals (get-topologically-sorted-signals))
  (for-each signal-update! (filter (compose not signal-up-to-date?) signals))
  (sleep 0.01)
  (update-signals-loop))

;; ==============================================
;; Lifting
;; One or more signals can be lifted with a procedure to create a new signal
;; Syntax: (lift (lambda (value1 value2 ...) ( ... )) $signal1 $signal2 ...)
;; ==============================================
(define (lift? exp) (tagged-list? exp 'lift))
(define (lift-operator exp) (cadr exp))
(define (lift-signals exp) (cddr exp))

(define (eval-lift operator-exp signal-exps env)
  (define operator (eval operator-exp env))
  (define wrapped-parents (map (lambda (signal-exp) (eval signal-exp env)) signal-exps))
  (define parents (map signal-wrapper-unwrap wrapped-parents)) 
  (define value-provider (lambda parent-values (apply-in-scope operator parent-values)))
  (make-signal-wrapper (make-signal parents value-provider)))
  
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
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;; ====================================
;;             PERFORMANCE
;; ====================================

;; CASE 1: Fan out signal
(define (create-fanout-signals)
  (eval '(define fanout-signal-0  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-1  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-2  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-3  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-4  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-5  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-6  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-7  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-8  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-9  (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-10 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-11 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-12 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-13 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-14 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-15 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-16 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-17 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-18 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-19 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-20 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-21 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-22 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-23 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-24 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-25 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-26 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-27 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-28 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-29 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-30 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-31 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-32 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-33 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-34 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-35 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-36 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-37 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-38 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-39 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-40 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-41 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-42 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-43 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-44 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-45 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-46 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-47 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-48 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-49 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-50 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-51 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-52 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-53 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-54 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-55 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-56 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-57 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-58 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-59 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-60 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-61 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-62 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-63 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-64 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-65 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-66 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-67 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-68 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-69 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-70 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-71 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-72 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-73 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-74 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-75 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-76 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-77 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-78 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-79 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-80 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-81 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-82 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-83 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-84 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-85 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-86 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-87 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-88 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-89 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-90 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-91 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-92 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-93 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-94 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-95 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-96 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-97 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-98 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
  (eval '(define fanout-signal-99 (lift (lambda (x) (save-string (string-append (number->string (current-milliseconds)) ";" (number->string x) " ")) x) $current-seconds)) the-global-environment)
)

(define ignore (create-fanout-signals))

;; keep built in signals up to date in separate threads
(display "Booting current-seconds loop") (newline)
(define currentsecondsthread (thread current-seconds-loop))
(display "Booting random-integer loop") (newline)
;;(thread random-integer-loop)
(display "Booting update-signals loop") (newline)
(define updatethread (thread update-signals-loop))
(display "Booting driver loop")
;;(driver-loop)
(display "Going to sleep for 10 seconds") (newline)
(sleep 10)
(display "Alright playtime's over, killing threads") (newline)
(kill-thread currentsecondsthread)
(kill-thread updatethread)
(for-each display strings)