#lang racket
; crear/explotar un contexte
(provide resolve full-resolve make-context add-to-context new-context)
(provide (struct-out geo-lambda) app)


(define (resolve context var)
  (hash-ref context var #f))

(define (assign-one ctx def)
  (match def
    [(list (? symbol? v) c) (hash-set! ctx v c)]
    [_ (error "Not conforming" def)]))
  
(define (make-context defs)
  (define ctx (make-hash))
  (for ([d defs]) (assign-one ctx d))
  ctx)


  
(define (new-context [fctx '()])
  (cons (make-hash) fctx))

(define (drop-context fctx)
  (rest fctx))

(define (add-to-context full-ctx defs)
  (let ([current-ctx (first full-ctx)])
    (for ([d defs]) (assign-one current-ctx d))))

(define (full-resolve full-ctx v)
  (let find ([stack full-ctx])
    (if (empty? stack)
        #f
        (or (resolve (first full-ctx) v)
            (find (rest full-ctx))))))

(struct geo-lambda [args steps])

(define (app fctx geo . args)
  (let ([local-ctx (new-context fctx)])
    (add-to-context local-ctx
                    (for/list ([v (geo-lambda-args geo)] [a args]) (list v a)))
    (values local-ctx (geo-lambda-steps geo))))
    
  
(define example
  (geo-lambda
   '(A B)
   '([r (len A B)]
     [c1 (circ A r)]
     [c2 (circ B r)]
     [CD (cut c1 c2)])))

(define-values (local const) (app (new-context) example '(point 0 0) '(point 1 0)))
(add-to-context local const)



