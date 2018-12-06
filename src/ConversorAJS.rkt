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
      [(list (? symbol? v) c) (hash-set! ctx v c)]
      [_ (error "Not conforming" def)]))
  (for ([d defs]) (assign-one d))
  ctx
)

(define example '(
[A (point 0 0)]
[B (point 1 0)]
[c1 (circ A 1)]
[c2 (circ B 1)]
[CD (cut c1 c2)]))

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
    [(list x e) (js-assign x (js-one e ctx))]
    [(list 'point x y) (format "hola1") (js-call "Point" x y)]
    [(list 'line x y) (js-call "line_from_points" x y)]
    [(list 'circ x y) (js-call "circle_from_center_radius" x y)]
    [(list 'vector x y) (js-call "Vector" x y)]
    [(list 'cut x y) (format "hola2")  (type x y ctx)]
    [_ ""])
    ;;(error "ups")]
  )


; funcio per calcular distancia entre punts per poder fer radis
(define (triangle)
  (define ctx (make-context example))
  (string-join
   (for/list ([e example])
     (js-one e ctx))
   "\n"))
;)




; Fer un match per mirar si son "line x y" o "circ x y" a la funcion type