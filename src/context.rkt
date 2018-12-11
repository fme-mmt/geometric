#lang racket
; crear/explotar un contexte
(provide resolve make-context)


(define (resolve context var)
  (hash-ref context var #f))

(define (make-context defs)
  (define ctx (make-hash))
  (define (assign-one def)
    (match def
      [(list (? symbol? v) c) (hash-set! ctx v c)]
      [_ (error "Not conforming" def)]))
  (for ([d defs]) (assign-one d))
  ctx)

