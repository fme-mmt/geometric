#lang racket
; crear/explotar un contexte
(provide resolve full-resolve make-context)


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


(define (make-full-context) '())
  
(define (new-context fctx)
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

(define (app fctx geo-lambda args)
  (let ([local-ctx (new-context fctx)]
        [actions (cdr geo-lambda)]
        [vars (car geo-lambda)])
    (add-to-context local-ctx
                    (for/list ([v vars] [a args]) (list v a)))
    (values local-ctx actions)))
    
    
  



