#lang racket
(require "algebra.rkt" json)

;; https://codepen.io/jsaludes/pen/qQVgKG

(define desmos-context
  (hash-remove
   (string->jsexpr
    "{\"viewport\":{\"size\":25,\"centerX\":0,\"centerY\":0},\"version\":\"4\",\"customTools\":{},\"graphSettings\":{\"showAxisNumbers\":false,\"showGrid\":true,\"showAxes\":true},\"objects\":{\"16\":{\"parents\":{\"point2\":\"14\",\"point1\":\"13\"},\"id\":\"16\",\"type\":\"line\",\"color\":\"#ff0000\",\"hidden\":false},\"13\":{\"parents\":{\"host1\":\"8\",\"host2\":\"3\"},\"id\":\"13\",\"type\":\"intersection\",\"color\":\"#c74440\",\"negRoot\":false,\"hidden\":false},\"1\":{\"parents\":{},\"x\":2.992596390982019,\"y\":-0.12116666743687254,\"id\":\"1\",\"type\":\"point\",\"color\":\"#c74440\",\"hidden\":false},\"0\":{\"parents\":{},\"x\":-3.350261581455111,\"y\":-0.03545237051204708,\"id\":\"0\",\"type\":\"point\",\"color\":\"#c74440\",\"hidden\":false},\"14\":{\"parents\":{\"host1\":\"8\",\"host2\":\"3\"},\"id\":\"14\",\"type\":\"intersection\",\"color\":\"#c74440\",\"negRoot\":true,\"hidden\":false},\"3\":{\"parents\":{\"point2\":\"1\",\"point1\":\"0\"},\"id\":\"3\",\"type\":\"circle\",\"color\":\"#383838\",\"hidden\":false},\"8\":{\"parents\":{\"point2\":\"0\",\"point1\":\"1\"},\"id\":\"8\",\"type\":\"circle\",\"color\":\"#388c46\",\"hidden\":false}}}")
   'objects))

(define (make-desmos-state objs)
  (hash-set desmos-context 'objects (elements objs)))


(define triangle '(
   [A (point 0 0)]
   [B (point 1 0)]
   [c1 (circ A B)]
   [c2 (circ B A)]
   [ab (line A B)]
   [cd (cut c1 c2)]
   [C (first cd)]
   [bc (line B C)]
   [ac (line A C)]))
   
(define mediatriu '([A (point 10 0)] [B (point 0 0)] [ca (circ A B)] [cb (circ B A)] [cd (cut ca cb)] [C1 (first cd)] [C2 (second cd)] [m (line C1 C2)]))
                    

(define (make-ids ctx)
  (for/hash ([k (hash-keys ctx)] [n (in-naturals)]) (values k (number->string n))))


(define (geo->desmos-objects exprs)
  (define ids (make-hash))
  (define vars (make-hash))
  (define (new o)
    (let ([id (get-id o)])
      (hash-set! ids (if (symbol? id) id (error "not a symbol")) o)
      id))
  (define (desmos expr) 
    (match expr
      [(? symbol? v) (hash-ref vars v)]
      [(? number? x) x]
      [`(point ,x ,y)
       (new (json-point (desmos x) (desmos y)))]
      [`(line ,p ,q)
       (new (json-line (desmos p) (desmos q)))]
      [`(circ ,p1 ,p2)
       (new (json-circle (desmos p1) (desmos p2)))]
      [`(cut ,a ,b)
       (λ (neg)
         (new (hash-set (json-intersection (desmos a) (desmos b)) 'negRoot neg)))]
      [`(first ,e)
       ((desmos e) #t)]
      [`(second ,e)
       ((desmos e) #f)]
      [`(second ,cut) (hash-set (desmos cut) 'negRoot #f)]
      [(list (? symbol? v) e)
       (let ([de (desmos e)])
         (hash-set! vars v
                    (cond
                      [(symbol? de) (symbol->string de)]
                      [else de])))]
      [_ (error "Cannot translate to desmos" expr)]))
  (for ([e exprs]) (desmos e))
  ids)

(define (set-size size dc)
  (let* ([vp 'viewport]
         [vp-hash (hash-ref dc vp)]
         [vp-hash* (hash-set vp-hash 'size size)])
    (hash-set dc vp vp-hash*)))

(define (w geo)
  (define desmos-context*
    (set-size 50
              (hash-set desmos-context
                        'graphSettings (hash-set* (hash-ref desmos-context 'graphSettings) 'showAxes #f ' showGrid #f))))
  (write
   (jsexpr->string
    (hash-set desmos-context* 'objects (geo->desmos-objects geo)))))
  



  



    