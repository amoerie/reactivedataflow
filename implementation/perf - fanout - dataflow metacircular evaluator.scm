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
    (define-variable! '$random-integer (make-signal-wrapper $random-integer) initial-env)
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
        (list 'string-append string-append)
        (list 'number->string number->string)
        (list 'current-milliseconds current-milliseconds)
        (list 'display display)
        (list 'newline newline)
        (list 'even? even?)
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
(define $random-integer (make-signal '() (lambda () (random 1 100))))
(define (random-integer-loop)
  (signal-up-to-date! $random-integer #f)
  (sleep 0.01)
  (random-integer-loop))

;; ==============================================
;; Signal graph + update loop
;; ==============================================
(define source-signals (list $current-seconds $random-integer))

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
(define (create-fanout-signals number-of-signals)
  (eval '(define (returnsame name)
           (lambda (value) value))
        the-global-environment)
  (eval '(define fanout-signal-0 (lift (returnsame "fanout-signal-0") $current-seconds)) the-global-environment)

  (define (apply-template new-signal-name previous-signal-name)
    ;; (define fanout-signal-3 (lift (plus1 "fanout-signal-3") fanout-signal-2))
    (quasiquote (define (unquote new-signal-name) (lift (returnsame (unquote (symbol->string new-signal-name))) (unquote previous-signal-name)))))

  (define (get-fanout-signal-name number)
    (string->symbol (string-append "fanout-signal-" (number->string number))))
  
  (define (create-fanout-signal current-number)
    (define current-signal-name (get-fanout-signal-name current-number))
    (define expression (apply-template current-signal-name 'fanout-signal-0))
    (eval expression the-global-environment))
                            
  (define (create-fanout-signal-loop n)
    (if (< n number-of-signals)
      (begin
        (create-fanout-signal n)
        (create-fanout-signal-loop (+ n 1)))
      #t))
  
  (create-fanout-signal-loop 1)
  (eval '(lift (lambda (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49 v50 v51 v52 v53 v54 v55 v56 v57 v58 v59 v60 v61 v62 v63 v64 v65 v66 v67 v68 v69 v70 v71 v72 v73 v74 v75 v76 v77 v78 v79 v80 v81 v82 v83 v84 v85 v86 v87 v88 v89 v90 v91 v92 v93 v94 v95 v96 v97 v98 v99)
                 (newline) (display (string-append (number->string (current-milliseconds)) ";" (number->string v99))) value) 
               fanout-signal-0 fanout-signal-1 fanout-signal-2 fanout-signal-3 fanout-signal-4 fanout-signal-5 fanout-signal-6 fanout-signal-7 fanout-signal-8 fanout-signal-9 fanout-signal-10 fanout-signal-11 fanout-signal-12 fanout-signal-13 fanout-signal-14 fanout-signal-15 fanout-signal-16 fanout-signal-17 fanout-signal-18 fanout-signal-19 fanout-signal-20 fanout-signal-21 fanout-signal-22 fanout-signal-23 fanout-signal-24 fanout-signal-25 fanout-signal-26 fanout-signal-27 fanout-signal-28 fanout-signal-29 fanout-signal-30 fanout-signal-31 fanout-signal-32 fanout-signal-33 fanout-signal-34 fanout-signal-35 fanout-signal-36 fanout-signal-37 fanout-signal-38 fanout-signal-39 fanout-signal-40 fanout-signal-41 fanout-signal-42 fanout-signal-43 fanout-signal-44 fanout-signal-45 fanout-signal-46 fanout-signal-47 fanout-signal-48 fanout-signal-49 fanout-signal-50 fanout-signal-51 fanout-signal-52 fanout-signal-53 fanout-signal-54 fanout-signal-55 fanout-signal-56 fanout-signal-57 fanout-signal-58 fanout-signal-59 fanout-signal-60 fanout-signal-61 fanout-signal-62 fanout-signal-63 fanout-signal-64 fanout-signal-65 fanout-signal-66 fanout-signal-67 fanout-signal-68 fanout-signal-69 fanout-signal-70 fanout-signal-71 fanout-signal-72 fanout-signal-73 fanout-signal-74 fanout-signal-75 fanout-signal-76 fanout-signal-77 fanout-signal-78 fanout-signal-79 fanout-signal-80 fanout-signal-81 fanout-signal-82 fanout-signal-83 fanout-signal-84 fanout-signal-85 fanout-signal-86 fanout-signal-87 fanout-signal-88 fanout-signal-89 fanout-signal-90 fanout-signal-91 fanout-signal-92 fanout-signal-93 fanout-signal-94 fanout-signal-95 fanout-signal-96 fanout-signal-97 fanout-signal-98 fanout-signal-99
         )
        the-global-environment)
)

(define ignore (create-fanout-signals 100))

;; keep built in signals up to date in separate threads
(display "Booting current-seconds loop") (newline)
(define currentsecondsthread (thread current-seconds-loop))
(display "Booting random-integer loop") (newline)
;;(thread random-integer-loop)
(display "Booting update-signals loop") (newline)
(define updatethread (thread update-signals-loop))
(display "Booting driver loop")
;;(driver-loop)
(display "Going to sleep for 60 seconds") (newline)
(sleep 60)
(display "Alright playtime's over, killing threads") (newline)
(kill-thread currentsecondsthread)
(kill-thread updatethread)