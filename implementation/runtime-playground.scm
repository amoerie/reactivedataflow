#lang racket

(require racket/date)
(require "runtime.rkt")
(include "runtime.rkt")

(define my-instructions (instructions 
  '(make-ro-store) 
  '(operation 1 (lambda (x) (res x)) (ports (port (link 2 0)))) 
  '(operation 1 (lambda (x) (res (+ x 1))) (ports (port (link 3 0))))
  '(make-ro 'val)))

(define dataflow-manager (start-runtimes my-instructions 1))
(add-inputs-with-return! dataflow-manager 1 '(123))
(define runtime-result (get-value! dataflow-manager))
(display runtime-result)