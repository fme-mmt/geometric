#lang racket
(struct point (x y))
(struct vector (x y))
(struct line (p q))
(struct circ (p q))


(define (js-number x)
  (cond
    [(number? x) (number->string x)] 
    [(symbol? x) (symbol->string x)]
    [else (error "Not a number")]
  )
)


(define (js-point p)
  (cond
    [(point? p) (format "Point(~a,~a)" (point-x p) (point-y p))]
    [else (error "Not a point")]
  )
)


(define (js-line l)
  (cond
    [(line? l) (cond
      [(point? (line-p l)) (cond
        [(point? (line-q l)) (format "line_from_points(Point(~a,~a),Point(~a,~a))" (point-x (line-p l)) (point-y (line-p l)) (point-x (line-q l)) (point-y (line-q l)))]
        [else (error "Not a line: the second elment is not a point")]
      )]
      [else (error "Not a line: the first element is not a point")]
    )]
    [else (error "Not a line")]
  )
)


(define (radius c) (sqrt ( + ( * ( - (point-x (circ-p c)) (point-x (circ-q c))) (- (point-x (circ-p c)) (point-x (circ-q c)))) ( * ( - (point-y (circ-p c)) (point-y (circ-q c))) ( - (point-y (circ-p c)) (point-y (circ-q c)))))))


(define (js-circ c)
  (cond
    [(circ? c) (cond
      [(point? (circ-p c)) (cond
        [(point? (circ-q c)) (format "circle_from_center_radius(Point(~a,~a),~a)" (point-x (circ-p c)) (point-y (circ-p c)) (radius c))]
        [else (error "Not a circle: the second elment is not a point")]
      )]
      [else (error "Not a circle: the first element is not a point")]
    )]
    [else (error "Not a circle")]
  )
)

(define (js-vector v)
   (cond
    [(vector? v) (format "Vector(~a,~a)" (vector-x v) (vector-y v))]
    [else (error "Not a vector")]
  )
)

(define (js-call string arg1 arg2 )
  (format "~a(~a,~a)" string arg1 arg2))

(define (js-assign a e)
  (format "var ~a = ~a;" a e))


(define (add-to-context ct expr)
  (match expr
    [(list (? symbol? v) e) (hash-set! ct v e)]
    [_ ct]))

; crear/explotar un contexte
(define (make-context defs)
  (define ctx (make-hash))
  (define (assign-one def)
    (match def
      [(list 'given vs ...)
       (for ([v vs])
         (hash-set! ctx v #f))]
      [(list (? symbol? v) c) (hash-set! ctx v c)]
      [_ (error "Not conforming" def)]))
  (for ([d defs]) (assign-one d))
  ctx
)

(define example '(
    [given A B]
    [c1 (circ A B)]
    [c2 (circ B A)]
    [CD (cut c1 c2)]
    [C (first CD)]
    [D (second CD)]
    [a (line B C)]
    [b (line C A)]
    [c (line A B)]
))

(define (resolve context var)
  (hash-ref context var #f))

(define (type x y ctx)
  (define X (resolve ctx x))
  (define Y (resolve ctx y))
  (match (list X Y)
    [(list (list 'line _ _) (list 'line _ _)) (js-call "intersect_lines" x y)]
    [(list (list 'line _ _) (list 'circ _ _)) (js-call "intersect_line_circle" x y)]
    [(list (list 'circ _ _) (list 'line _ _)) (js-call "intersect_line_circle" y x)]
    [(list (list 'circ _ _) (list 'circ _ _)) (js-call "intersect_circles" x y)]
    [_ (error "Can't intersect")]
  )
)
    

;(define (eqs ctx v)
;  (match (resolve ctx v)
;    [#f (error "Not defined" v)]
;    [(list 'cut c1 c2) ((eqs-intersection (eqs ctx c1) (eqs ctx c2)))]
;    [(list 'circ p r) (eqs-circle (eqs ctx p) (eqs ctx r))]
;    [(list 'len p q) (dist-points (eqs ctx p) (eqs ctx q))]
;    [(list 'point a b) (eqs-point a b)]
;    [_ (error "not found" v)]
;  )
;)

;canviar int_l, int_l_c i int_c per una unica funcio cut i distingir entre que cal cridar amb el hash (l&l, l&c, c&l, c&c)
;(make-context '((A (point 0 0)) (B (point 1 0)) (L (line A B)))
;(js-one '(A (point a b)))
(define (js-one expr ctx)
  (match expr
    [(list 'given vs ...) ""]
    [(list 'point x y) (js-call "Point" x y)]
    [(list 'line x y) (js-call "line_from_points" x y)]
    [(list 'circ x y) (js-call "circle_from_center_radius" x (js-call "dist" x y))]
    [(list 'vector x y) (js-call "Vector" x y)]
    [(list 'cut x y) (type x y ctx)]
    [(list 'first x) (format "~a[~a]" x "0")]
    [(list 'second x) (format "~a[~a]" x "1")]
    [(list (? symbol? x) e) (js-assign x (js-one e ctx))]
    [_  (error "ups" expr)])
  )

(define (js-draw expr)
  (match expr
    [(list 'given vs ...)
     (string-join (for/list ([a vs]) (js-call-drawpoint a)) "\n")]
    [(list 'first x) (js-call-drawpoint x)]
    [(list 'second x) (js-call-drawpoint x)]
    [(list 'line x) (js-call-line x)]
    [(list 'circ x)(js-call-circ x)]
    [(list 'cut x) ""]
    [(list (? symbol? x) e) (js-draw (list (first e) x))]
    [_  (error "ups" expr)])
)



(define (js-call-drawpoint a)
  (format "drawpoint(\"~a\",~a,ctx);" a a))

(define (js-call-line l)
  (format "drawline(~a,ctx);" l))

(define (js-call-circ c)
  (format "drawcircle(~a,ctx,'black');" c))

(define (make-bloc-3 construct)
  (string-join
   (for/list ([e construct])
     (js-draw e))
   "\n"))

; funcio per calcular distancia entre punts per poder fer radis
(define (make-bloc-2 ctx construct)
  (string-join
   (for/list ([e construct])
     (js-one e ctx))
   "\n"))

(define (make-bloc-1 ctx)
  (string-join
   (for/list ([(v e) ctx] #:when (eq? e #f))
     (format "var ~a = Point(data.~ax, data.~ay);" v v v))
   "\n"))

(define (make-data ctx)
  (string-append
   (format "var initialData = { \n")
   (string-join
    (for/list ([(v e) ctx] #:when (eq? e #f))
      (format "~ax : ~a,\n ~ay : ~a" v (random) v (random)))
    ",\n")
   (format "\n};\n")))
  
(define (triangle)
    (define ctx (make-context example))
  (string-append
   (make-data ctx)
   (make-bloc-1 ctx)
   (make-bloc-2 ctx example)))

(define (write-to-file path)
  (call-with-output-file path
    (lambda (output-port)
      (display (triangle) output-port))))


(define (update-data path ctx)
  (call-with-input-file path
    (lambda (in)
      (let* ((input (port->string in)))
      (call-with-output-file path 
          #:exists 'replace
        (lambda (output-port)
          (display (string-replace input "/*initialData*/" (make-data ctx)) output-port)))))))


(define (update-1 path ctx)
  (call-with-input-file path
    (lambda (in)
      (let* ((input (port->string in)))
      (call-with-output-file path 
          #:exists 'replace
        (lambda (output-port)
          (display (string-replace input "/*bloc1*/" (make-bloc-1 ctx)) output-port)))))))


(define (update-2 path ctx example)
  (call-with-input-file path
    (lambda (in)
      (let* ((input (port->string in)))
      (call-with-output-file path 
          #:exists 'replace
        (lambda (output-port)
          (display (string-replace input "/*bloc2*/" (make-bloc-2 ctx example)) output-port)))))))

(define (update-3 path example)
  (call-with-input-file path
    (lambda (in)
      (let* ((input (port->string in)))
      (call-with-output-file path 
          #:exists 'replace
        (lambda (output-port)
          (display (string-replace input "/*bloc3*/" (make-bloc-3 example)) output-port)))))))

(define (update path)
  (define ctx (make-context example))
  (update-data path ctx)
  (update-1 path ctx)
  (update-2 path ctx example)
  (update-3 path example))
  