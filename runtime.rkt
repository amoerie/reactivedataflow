#lang racket

(require data/queue)

; ------- ;
; Runtime ;
; ------- ;

(struct runtime (
  id
  ; Pipeline components
  token-queue
  matcher
  ; Operation components
  context-memory
  context-manager))

(define (make-runtime id instruction-memory)
  (let ((token-queue     (make-token-queue))
        (matcher         (make-matcher))
        (context-memory  (make-context-memory))
        (context-manager (make-context-manager id)))
    (set-matcher-lookup!
      matcher
      (lambda (t)  (inst-in (mem-get instruction-memory (token-addr t)))))
    (set-matcher-trigger!
      matcher
      (lambda (data addr cont)
        (let* ((inst (inst-exec (mem-get instruction-memory addr)))
               (res
                  (inst
                    data
                    cont
                    (lambda () (matcher-read matcher cont))
                    (lambda () context-manager)
                    (lambda () (context-memory-get-cont! context-memory cont)))))
          (for-each (lambda (t) (add-token! token-queue t)) res))))
    (runtime id token-queue matcher context-memory context-manager)))

(define (runtime-finished? rt)
  (token-queue-empty? (runtime-token-queue rt)))

(define (runtime-add-token! rt token)
  (add-token! (runtime-token-queue rt) token))

(define (runtime-process-token! rt)
  (define tok (dequeue! (runtime-token-queue rt)))
  (matcher-add-token! (runtime-matcher rt) tok))

(define (runtime-add-inputs! rt entry-point inputs)
  (let* ((cont (context-manager-get! (runtime-context-manager rt)))
         (args (map (lambda (datum idx) (make-token datum entry-point idx cont))
                    inputs (range (length inputs)))))
    (for-each (lambda (tok) (runtime-add-token! rt tok)) args)))

(define (run-program instruction-memory entry-point . inputs)
  (define rt (make-runtime 0 instruction-memory))
  (runtime-add-inputs! rt entry-point inputs)
  (let loop ()
    (runtime-process-token! rt)
    (when (not (runtime-finished? rt))
          (loop))))

; ----- ;
; Token ;
; ----- ;

; Interface
(struct token (datum addr port cont))
(define make-token token)

(define (get-token-data tokens)
  (map token-datum tokens))

; -------- ;
; Contexts ;
; -------- ;

(define (szudzik-pair x y)
  (if (>= x y)
    (+ (* x x) x y)
    (+ (* y y) x)))

(define (szudzik-unpair z)
  (let* ((roundedsq (floor (sqrt z)))
         (squared   (* roundedsq roundedsq))
         (minus     (- z squared)))
    (if (>= minus roundedsq)
      (cons (minus roundedsq))
      (cons (roundedsq (- minus roundedsq))))))

(define (create-context id offset) (szudzik-pair id offset))
(define (get-runtime-id c) (car (szudzik-unpair c)))

; ----------- ;
; Token Queue ;
; ----------- ;

(define make-token-queue make-queue)
(define token-queue-empty? queue-empty?)
(define add-token! enqueue!)
(define get-token! dequeue!)

; ------- ;
; Matcher ;
; ------- ;

(struct matcher (memory (lookup #:mutable) (trigger #:mutable)))

(define (make-matcher)
  (matcher (make-hash) (void) (void)))

(define (matcher-slot-ready? slot)
  (= (vector-count void? slot) 0))

(define (matcher-trigger-slot m t slot)
  ((matcher-trigger m) (vector->list slot) (token-addr t) (token-cont t)))

(define (matcher-get-cmem! m t)
  (hash-ref! (matcher-memory m) (token-cont t) make-hash))

(define (matcher-create-slot m t)
  (make-vector ((matcher-lookup m) t) (void)))

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

; Interface
(define (matcher-read m t)
  (matcher-get-cmem! m t))

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

(struct inst (in exec))

(define make-inst inst)

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
    (lambda (data cont _matching-memory _context-manager _context-memory)
      (let* ((res  (apply proc data)))
        (create-tokens res cont links)))))

; Switches
; --------

(define (make-sw in branches)
  (make-inst
    in
    (lambda (data cont _matching-memory _context-manager _context-memory)
      (let* ((idx  (car data))
             (dst  (vector-ref branches idx)))
        (create-tokens data cont dst)))))

; Calls
; -----

(define (make-cl in dst-links ret-links)
  (make-inst
    in
    (lambda (data old-cont _matching-memory context-manager context-memory)
      (let ((new-cont (context-manager-get! (context-manager)))
            (memory   (context-memory)))
        (list*
          (token ret-links 0 0 new-cont)
          (token old-cont  0 1 new-cont)
          (create-tokens   data new-cont dst-links))))))

(define (make-call-store)
  (make-inst
    2
    (lambda (data cont _matching-memory _context-manager context-memory)
      (let* ((memory    (context-memory))
             (links     (car  data))
             (ret-cont  (cadr data)))
        (context-memory-put! memory 'ret-links links)
        (context-memory-put! memory 'ret-cont  ret-cont))
      '())))

(define (make-rt in)
  (make-inst
    in
    (lambda (data current _matching-memory context-manager context-memory)
      (let* ((memory  (context-memory))
             (return  (context-memory-get memory 'ret-cont))
             (links   (context-memory-get memory 'ret-links)))
        (context-manager-recycle! (context-manager) current)
        (create-tokens data return links)))))

