#lang racket
(require "context.rkt")
(provide define/geo)

(define-syntax define/geo
  (syntax-rules ()
    [(_ (id v ...) e ...)
     (define id (geo-lambda `(v ...) `(e ...)))]
    [(_ id e ...)
     (define id `(e ...))]))

(module+ test
  (require rackunit)
  
  (define/geo tri
    [A (point 1 0)]
    [B (point 0 1)]
    [ca (circ A B)]
    [cb (circ B A)]
    [C (first (cut ca cb))]
    (polygon A B C))

  (displayln tri)

  (define/geo (midpoint A B)
    [ca (circ A B)]
    [cb (circ B A)]
    [cd (cut ca cb)]
    [l (line (first cd) (second cd))]
    (first (cut l (line A B))))

  (check-pred geo-lambda? midpoint))
  
  

