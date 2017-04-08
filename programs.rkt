#lang racket

(include "runtime.rkt")

; ------------ ;
; Abstractions ;
; ------------ ;

; Instruction types
; -----------------

(define operation make-op)
(define switch make-sw)
(define call make-cl)
(define ret make-rt)

;; DOCS

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

; Returning values
; ----------------

(define res list)

; ---------------- ;
; Example Programs ;
; ---------------- ;

; Basic examples
; --------------

; Returns sum of inputs and top
(define test-operation (instructions
                        (operation 2 (lambda (a b) (res (+ a b) 'top))
                                   (ports
                                    (port (link 1 0))
                                    (port (link 2 0))))
                        (operation 1 (lambda (x) (display x) (res)) (ports ))
                        (operation 1 (lambda (x) (display x) (res)) (ports ))))

; Return idx of selected branch
(define test-switch (instructions
                     (operation 1 (lambda (b) (if b (res 0) (res 1)))
                                (ports (port (link 1 0))))
                     (switch 1
                             (branches
                              (ports (port (link 2 0)))
                              (ports (port (link 3 0)))))
                     (operation 1 (lambda (x) (display x) (res)) (ports ))
                     (operation 1 (lambda (x) (display x) (res)) (ports ))))

(define test-call (instructions
                   (make-call-store)
                   (call 1
                         (ports (port (link 3 0)))
                         (ports (port (link 2 0))))

                   (operation 1 (lambda (x) (display x) (res)) (ports ))

                   (operation 1 (lambda (x) (display "called with: ") (display x) (res x))
                              (ports (port (link 4 0))))
                   (ret 1)))

; Factorial
; ---------

(define (fac n) (if (> n 0) (* n (fac (- n 1))) 1))

(define factorial (instructions
                   (make-call-store)
                   (operation 1 (lambda (n) (res n))
                              (ports
                               (port (link 2 0) (link 4 1))))
                   (operation 1 (lambda (n) (res (> n 0)))
                              (ports
                               (port (link 3 0))))
                   (operation 1 (lambda (b) (res (if b 0 1)))
                              (ports
                               (port (link 4 0))))
                   (switch 2
                           (branches
                            (ports ;True branch
                             (port)
                             (port (link 5 0) (link 7 0)))
                            (ports ; False branch
                             (port (link 8 0))
                             (port))))
                   (operation 1 (lambda (n) (res (- n 1)))
                              (ports
                               (port (link 6 0))))
                   (call 1
                         (ports (port (link 1 0)))  ; Destination
                         (ports (port (link 7 1)))) ; Return location
                   (operation 2 (lambda (a b) (res (* a b)))
                              (ports
                               (port (link 9 0))))
                   (operation 1 (lambda (_) (res 1))
                              (ports
                               (port (link 9 0))))
                   (ret 1)

                   ; "Main" function
                   (call 1
                         (ports (port (link 1  0)))
                         (ports (port (link 11 0))))
                   (operation 1 (lambda (x) (display x) (res))
                              (ports ))))

; Fibonacci
; ---------

(define fibonacci (instructions
                   (make-call-store)
                   ; Main
                   (call 1
                         (ports (port (link 3 0) (link 5 1)))
                         (ports (port (link 2 0))))
                   (operation 1 (lambda (x) (display x) (res))
                              (ports ))

                   ; Fib
                   (operation 1 (lambda (n) (res (< n 2)))
                              (ports (port (link 4 0))))
                   (operation 1 (lambda (b) (res (if b 0 1)))
                              (ports (port (link 5 0))))
                   (switch 2
                           (branches
                            (ports (port ) (port (link 11 0)))              ; True branch
                            (ports (port ) (port (link 6  0) (link 8 0))))) ; False branch
                   (operation 1 (lambda (n) (res (- n 1)))
                              (ports (port (link 7 0))))
                   (call 1
                         (ports (port (link 3  0) (link 5 1)))
                         (ports (port (link 10 0))))
                   (operation 1 (lambda (n) (res (- n 2)))
                              (ports (port (link 9 0))))
                   (call 1
                         (ports (port (link 3  0) (link 5 1)))
                         (ports (port (link 10 1))))
                   (operation 2 (lambda (x y) (res (+ x y)))
                              (ports (port (link 11 0))))
                   (ret 1)))

(run-program test-operation 0 3 2) (newline)
(run-program test-switch 0 false) (newline)
(run-program test-call 1 5) (newline)
(run-program factorial 10 5) (newline)
(newline)
(run-program fibonacci 1 20) (newline)

;; 

(define is-even?
  (instructions
   ;; operation with index 0 : lambda that turns number into a boolean
   (operation 1
              (lambda (n) (res (even? n)))    ;; turn 3 -> false, 4 -> true
              (ports (port (link 1 0))) ;; send the output to instruction with index 1, argument index 0
              )
   ;; operation with index 1 : show the result
   (operation 1 (lambda (x) (display x) (res)) (ports ))
   ;; Return 1 .. all the other cool programs do it :P
   (ret 1)))

(newline)
(display "Is even?") (newline)
(run-program is-even? 0 13)
  