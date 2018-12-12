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
  (cond
    [(string=? string "Point") (cond
      [(number? arg1) (cond
        [(number? arg2) (format "~a(~a,~a)" string arg1 arg2)])]
      [else (error "The arguments don't match the definition of the function")])]
    [(string=? string "Vector") (cond
      [(number? arg1) (cond
        [(number? arg2) (format "~a(~a,~a)" string arg1 arg2)])]
      [else (error "The arguments don't match the definition of the function")])]
    [(string=? string "line_from_points") (cond
      [(point? arg1) (cond
        [(point? arg2) (format "~a(~a,~a)" string (js-point arg1) (js-point arg2))])]
      [else (error "The arguments don't match the definition of the function")])]
    [(string=? string "circle_from_center_radius") (cond
      [(point? arg1) (cond
        [(number? arg2) (format "~a(~a,~a)" string (js-point arg1) arg2)])]
      [else (error "The arguments don't match the definition of the function")])]
    [(string=? string "intersect_lines") (cond
      [(line? arg1) (cond
        [(line? arg2) (format "~a(~a,~a)" string (js-line arg1) (js-line arg2))])]
      [else (error "The arguments don't match the definition of the function")])]
    [(string=? string "intersect_line_circle") (cond
      [(line arg1) (cond
        [(circ? arg2) (format "~a(~a,~a)" string (js-line arg1) (js-circ arg2))])]
      [else (error "The arguments don't match the definition of the function")])]
    [else (error "There isn't a funciton with this name")]
  )
)
