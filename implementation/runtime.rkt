#lang racket

(require racket/place)
(require racket/future)

(provide (all-defined-out))

; ---------------- ;
; Eval magic stuff ;
; ---------------- ;

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

; ------------------ ;
; Verify Parallelism ;
; ------------------ ;

; Warn the user if the racket installation does not
; provide parallel futures
(unless (place-enabled?)
  (display "Places are not supported in your racket installation, ")
  (display "parallelism cannot be provided.")
  (newline))

; ------- ;
; Manager ;
; ------- ;

(struct manager (cores memory channel root-manager managers places))

(define (start-runtimes memory cores)
  (define-values (chan-in chan-out) (place-channel))
  (manager
   cores memory chan-in
   (make-context-manager -1)
   (build-vector cores (lambda (id) (make-context-manager id)))
   (build-vector cores (lambda (id) (create-runtime id chan-out memory)))))

(define (create-runtime id com memory)
  (define chan (place c (place-start c)))
  (place-channel-put chan id)
  (place-channel-put chan com)
  (place-channel-put chan memory)
  chan)

(define (stop-runtimes manager)
  (vector-map
   (lambda (c)
     (place-channel-put c 'stop)
     (place-wait c))
   (manager-places manager)))

(define (get-root-context! manager)
  (context-manager-get! (manager-root-manager manager)))

(define (get-context! manager)
  ; Provide a context for a random runtime
  (context-manager-get!
   (vector-ref (manager-managers manager) (random (manager-cores manager)))))

(define (add-token! manager token)
  (place-channel-put
   (vector-ref (manager-places manager) (get-runtime-id (token-cont token)))
   token))

(define (add-tokens! manager tokens)
  ; Add a list of tokens to a runtime determined by their context
  (for-each (lambda (t) (add-token! manager t)) tokens))

(define (add-inputs! manager entry-point inputs)
  ; Process a bunch of inputs with the same context.
  ; The tokens will be sent to "entry-point", each token will receive
  ; a port identical to it's position in the list
  (define cont (get-context! manager))
  (add-tokens!
   manager
   (map
    (lambda (input idx) (make-token input entry-point idx cont))
    inputs (range (length inputs)))))

(define (add-inputs-with-return! manager entry-point inputs)
  ; Process a bunch of inputs with the same context.
  ; The tokens will be sent to "entry-point", each token will receive
  ; a port identical to it's position in the list
  ; Furthermore create a root context and send it to the instruction 0,
  ; port 0. Return this root context
  (define cont (get-context! manager))
  (define root (get-root-context! manager))
  (add-token! manager (make-token root 0 0 cont))
  (add-tokens!
   manager
   (map
    (lambda (input idx) (make-token input entry-point idx cont))
    inputs (range (length inputs))))
  root)

(define (get-value! manager)
  ; Get the latest value which has been produced by any runtime
  ; This operation blocks!
  (define tok (place-channel-get (manager-channel manager)))
  (list (token-datum tok) (token-cont tok) (token-addr tok)))

; ---------------- ;
; Place Definition ;
; ---------------- ;

(define (place-start chan)
  (define id  (place-channel-get chan))
  (define com (place-channel-get chan))
  (define mem (eval-instructions (place-channel-get chan)))
  (place-loop chan (make-runtime id com mem)))
  
(define (eval-instructions mem)
  (vector-map (lambda (inst) (eval inst ns)) mem))

(define (place-loop chan rt)
  (define msg (place-channel-get chan))
    (unless (eq? msg 'stop)
      (runtime-process-token! rt msg)
      (place-loop chan rt)))

; ------- ;
; Runtime ;
; ------- ;

(struct runtime (
  id
  channel
  instruction-memory
  
  matcher
  context-memory))

(define (make-runtime id channel instruction-memory)
  (define r
    (runtime
     id
     channel
     instruction-memory
     (make-matcher)
     (make-context-memory)))
  (set-matcher-runtime! (runtime-matcher r) r)
  r)

(define (trigger! rt data addr cont)
  (define inst (inst-exec (mem-get (runtime-instruction-memory rt) addr)))
  (for-each
   (lambda (t) (runtime-process-token! rt t))
   (inst
    data
    cont
    (lambda () (context-memory-get-cont! (runtime-context-memory rt) cont)))))

(define (runtime-process-token! rt tok)
  (if (= (runtime-id rt) (get-runtime-id (token-cont tok)))
      (matcher-add-token! (runtime-matcher rt) tok)
      (place-channel-put  (runtime-channel rt) tok)))

; ----- ;
; Token ;
; ----- ;

(define (make-token datum addr port cont)
  (vector datum addr port cont))

(define token make-token)

(define (token-datum t) (vector-ref t 0))
(define (token-addr t)  (vector-ref t 1))
(define (token-port t)  (vector-ref t 2))
(define (token-cont t)  (vector-ref t 3))

; -------- ;
; Contexts ;
; -------- ;

(define (create-context id offset) (cons id offset))
(define (get-runtime-id c) (car c))

; ------- ;
; Matcher ;
; ------- ;

(struct matcher (memory (runtime #:mutable)))

(define (make-matcher) (matcher (make-hash) (void)))

(define (matcher-slot-ready? slot)
  (= (vector-count void? slot) 0))

(define (matcher-trigger-slot m t slot)
  (trigger!
    (matcher-runtime m)
    (vector->list slot)
    (token-addr t)
    (token-cont t)))

(define (matcher-get-cmem! m t)
  (hash-ref! (matcher-memory m) (token-cont t) make-hash))

(define (matcher-create-slot m t)
  (make-vector
    (inst-in
      (mem-get (runtime-instruction-memory (matcher-runtime m)) (token-addr t)))
    (void)))

(define (matcher-get-slot! m cmem t)
  (hash-ref! cmem (token-addr t) (lambda () (matcher-create-slot m t))))

(define (matcher-add-to-slot! slot t)
  (vector-set! slot (token-port t) (token-datum t)))

(define (matcher-clean-slot! m cmem t)
  (hash-remove! cmem (token-addr t))
  (when (hash-empty? cmem)
    (hash-remove! (matcher-memory m) (token-cont t))))

(define (matcher-add-token! m t)
  (let* ((cmem (matcher-get-cmem! m t))
         (slot (matcher-get-slot! m cmem t)))
    (matcher-add-to-slot! slot t)
    (when (matcher-slot-ready? slot)
      (matcher-trigger-slot m t slot)
      (matcher-clean-slot!  m cmem t))))

; --------------- ;
; Context Manager ;
; --------------- ;

(struct context-manager (id (ctr #:mutable)))

(define (make-context-manager id)
  (context-manager id 0))

; Interface
(define (context-manager-get! cm)
  (define ctr (context-manager-ctr cm))
  (set-context-manager-ctr! cm (+ ctr 1))
  (create-context (context-manager-id cm) ctr))

; Interface
(define (context-manager-recycle! cm c) (void))

; -------------- ;
; Context Memory ;
; -------------- ;

(define (make-context-memory)
  (make-hash))

(define (context-memory-get-cont! cm cont)
  (hash-ref! cm cont make-hash))

; Interface
(define (context-memory-get cm key)
  (hash-ref cm key))

; Interface
(define (context-memory-put! cm key val)
  (hash-set! cm key val))

; ------------ ;
; Instructions ;
; ------------ ;

(define (make-inst in exec)
  (cons in exec))

(define (inst-in inst)
  (car inst))

(define (inst-exec inst)
  (cdr inst))

; Wiring
; ------

(define link cons)
(define link-addr car)
(define link-port cdr)

(define port  list)
(define port-for-each-link for-each)

(define ports list)
(define for-each-port for-each)

(define branches vector)
(define branch-get vector-ref)

; Instruction Memory
; ------------------

(define instructions vector)
(define mem-size vector-length)
(define mem-get  vector-ref)

; Util
; ----

(define (create-tokens data cont links)
  (flatten
    (map
      (lambda (datum port)
        (map
          (lambda (l) (make-token datum (link-addr l) (link-port l) cont))
          port))
        data links)))

; Operations
; ----------

(define (make-op in proc links)
  (make-inst
    in
    (lambda (data cont _context-memory)
      (let* ((res  (apply proc data)))
        (create-tokens res cont links)))))

; Switches
; --------

(define (make-sw in branches)
  (make-inst
    in
    (lambda (data cont _context-memory)
      (let* ((idx  (car data))
             (dst  (vector-ref branches idx)))
        (create-tokens data cont dst)))))

; Returning to outside world
; --------------------------

(define (make-ro-store)
  (make-inst
    1
    (lambda (data cont context-memory)
      (let ((memory    (context-memory)))
        (context-memory-put! memory 'ret (car data)))
      '())))

(define (make-ro tag)
  (make-inst
    1
    (lambda (data current context-memory)
      (let* ((memory  (context-memory))
             (cont  (context-memory-get memory 'ret)))
        (list (token (car data) tag 0 cont))))))

; --------- ;
; Test Code ;
; --------- ;

; Instruction types
; -----------------

(define operation make-op)

; Returning values
; ----------------

(define res list)


; Stupid test to see if basics work
(define test-operation (instructions
    '(operation 2 (lambda (a b) (res (+ a b) 'top))
        (ports
            (port (link 1 0))
            (port (link 2 0))))
    '(operation 1 (lambda (x) (display x) (res)) (ports ))
    '(operation 1 (lambda (x) (display x) (res)) (ports ))))

; Test to check communication with outside world
(define test-communication
  (instructions
   '(make-ro-store)
   '(operation 2 (lambda (a b) (res (+ a b)))
               (ports (port (link 2 0))))
   '(make-ro 'val)))

; How to use:
; 1. Create manager: (define m (start-runtimes test-communication 8))
; 2. Send inputs to manager: (add-inputs-with-return! m 1 '(4 5)).
;    -> this returns a unique context!
; 3. Ask the runtime for new values (blocking): (get-value! m)
;    -> This returns a token with a context generated by add-inputs-with-return,
;       the instruction is a tag assigned by make-ro (useful to figure out from which
;       instruction a token came) and a useles port
