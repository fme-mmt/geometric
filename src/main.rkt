#lang racket
(require
  "context.rkt"
  (rename-in "algebra.rkt" (pretty-print pp) (eqs eqs*)))

(define-syntax define/geo
  (syntax-rules ()
    [(_ (id v ...) e ...)
     (define id (geo-lambda `(v ...) `(e ...)))]
    [(_ id e ...)
     (define id `(e ...))]))

(define (equations g [vars '(x y)])
  (match g
    [(list ve ... fe)
     (let* ([ctx (new-context)])
       (add-to-context ctx ve)
       (apply (eqs* ctx fe) vars))]
    [_ (error "Not conforming to geo" g)])) 


(module+ test
  (require rackunit)

  (define/geo (midpoint A B)
    (ca (circ A B))
    [cb (circ B A)]
    [cd (cut ca cb)]
    [C (first cd)]
    [D (second cd)]
    (first (intersect (line c d) (line A B))))

  (define/geo triangle
    [A (point 0 0)]
    [B (point 1 0)]
    [r (len A B)]
    [c (circ A r)]
    c)
  
  (displayln triangle)
  (check-pred geo-lambda? midpoint)

 (for ([e (equations triangle)])
       (displayln (pp e))))

    