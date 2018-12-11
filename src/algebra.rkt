#lang racket

(require json)
(require "context.rkt")

(provide eqs maple-solve eqs-point eqs-line eqs-circle eqs-intersection dist-points explain)
(provide elements json-point json-line json-circle json-intersection get-id pretty-print)

(define indexs (make-hash))
(define (new-var [prefix 'x])
  (hash-update! indexs prefix add1 0)
  (string->symbol
   (format "~a_~a" prefix (hash-ref indexs prefix))))

; equacions del punt
(define (eqs-point a b)
  (λ (x y)
    `((- ,x ,a) (- ,y ,b))))


; equacions de la recta
(define (eqs-line eqs-p eqs-q)
  (λ (x y)
    (define ap (new-var 'a))
    (define bp (new-var 'b))
    (define aq (new-var 'a))
    (define bq (new-var 'b))
    (append (eqs-p ap bp)
            (eqs-q aq bq)
            (list `(- (* (- ,y ,bp) (- ,aq ,ap)) (* (- ,x ,ap) (- ,bq ,bp)))))))


; equacions cercle
(define (eqs-circle eqs-p eqs-r)
  (λ (x y)
    (define a (new-var 'a))
    (define b (new-var 'b))
    (define r (new-var 'r))
    (append (eqs-p a b)
            (eqs-r r)
            (list `(- (+ (sqr (- ,x ,a)) (sqr (- ,y ,b))) (sqr ,r))))))

; equacions interseccio
(define (eqs-intersection obj-1 obj-2)
  (λ (x y)
    (append (obj-1 x y) (obj-2 x y))))

; distancia entre dos punts
(define (dist-points eqs-p eqs-q)
  (λ (d)
    (define ap (new-var 'a))
    (define bp (new-var 'b))
    (define aq (new-var 'a))
    (define bq (new-var 'b))
    (append (eqs-p ap bp)
            (eqs-q aq bq)
            (list `(- (+ (sqr (- ,ap ,aq)) (sqr (- ,bp ,bq))) (sqr ,d))))))

; triangle equilater
;(define (triangle-equilater eqs-p eqs-q)
;  (λ (x y)
;    (append ((dist-points eqs-p eqs-q) 'd)
;            ((eqs-circle eqs-p d) 'x 'y)
;            ((eqs-circle eqs-q d) 'x 'y)
;            ((eqs-intersection ((eqs-circle eqs-p d) 'x 'y) ((eqs-circle eqs-q d) 'x 'y)) 'x 'y))))
;
;(define A (eqs-point 0 0))
;(define B (eqs-point 0 1))



(define example '(
   [A (point 0 0)]
   [B (point 1 0)]
   [r (len A B)]
   [c1 (circ A r)]
   [c2 (circ B r)]
   [CD (cut c1 c2)]))
    

(define (eqs ctx v)
    (match (resolve ctx v)
      [#f (error "Not defined" v)]
      [(list 'cut c1 c2) ((eqs-intersection (eqs ctx c1) (eqs ctx c2)))]
      [(list 'circ p r) (eqs-circle (eqs ctx p) (eqs ctx r))]
      [(list 'len p q) (dist-points (eqs ctx p) (eqs ctx q))]
      [(list 'point a b) (eqs-point a b)]
      [_ (error "not found" v)]))


;pretty print
(define (pretty-print p)
  (car (pp p)))

(define (pp pol)
  (define (parens-if sp p)
     (let ([pf (cdr sp)]
           [sf (car sp)])
       (if (<= pf p) (string-append "(" sf ")") sf)))
  (match pol
    [(? number? n) (cons (number->string n) 9)]
    [(? symbol? x) (cons (symbol->string x) 9)]
    [(list '* fs ...)
     (cons
      (string-join
      (for/list ([f fs])
        (parens-if (pp f) 3))
      "*")
      3)]
    [(list '+ fs ...)
     (cons
      (string-join
      (for/list ([f fs])
        (parens-if (pp f) 2))
      "+")
      2)]
    [(list '- fs ...)
     (cons
      (string-join
      (for/list ([f fs])
        (parens-if (pp f) 2))
      "-")
      2)]
    [(list 'sqr x)
     (cons
      (string-append (parens-if (pp x) 4) "^2")
     4)]
    [_ (error "Not conforming" pol)]))

(define (maple pols)
  (maple-list
   (map (λ (p) (string-append (pretty-print p) "=0"))  pols)))

(define (maple-list ls)
  (string-append "{"
                 (string-join ls  ", ") "}"))

(define (maple-solve pols)
  (define var (vars pols)) ;[vars '(x y)])
  (string-append "solve(" (maple pols) ", " (maple-list (map symbol->string (set->list var))) ")"))

(define (WolframAlpha pols)
  (define var (vars pols)) ;[vars '(x y)])
  (string-append "GroebnerBasis[" (maple pols) ", " (maple-list (map symbol->string (set->list var))) "]"))

;variables polinomi
(define (vars-eq equ)
  (match equ
    [(? symbol? n) (set n)]
    [(list (? symbol?) args ...) (apply set-union (map vars-eq args))]
    [_ (set)]))
 
(define (vars eqs) ; on eqs ha de ser una llista d'equacions
  (apply set-union (map vars-eq eqs)))


;Midterm presentation
    ;triangle equilater
      (define A (eqs-point 0 0))
      (define B (eqs-point 1 0))
      (define dist (dist-points A B))
      (define c1 (eqs-circle A dist))
      (define c2 (eqs-circle B dist))
      (define intersectionCD (eqs-intersection c1 c2))
      ;; (maple-solve (intersectionCD 'x 'y))

;Wolfram alpha
;APP NAME: RacketUPC
;APPID: RYA2KG-Q4JE928RYV

; https://docs.racket-lang.org/reference/Writing.html
; https://docs.racket-lang.org/guide/read-write.html

; (fprintf (current-output-port)
;            "~a as a string is ~s.\n"
;           '(3 4)
;            "(3 4)")

; descripcio dels passos
(define (description1 cmd)
  (match cmd
    [(list (? symbol? x) p) (format "Sigui ~a ~a" x (description1 p))]
    [(list 'point x y) (format "el punt de coordenada x = ~a i coordenada y = ~a." x y)]
    [(list 'len x y) (format "la distància entre ~a i ~a." x y)]
    [(list 'circ x y) (format "la circumferència de centre ~a i radi ~a." x y)]
    [(list 'cut x y) (format "la intersecció entre ~a i ~a." x y)]))

(define (description2 cmds) ; on cmds ha de ser una llista d'elements
  ; (apply string-append (map description1 cmds)))
  (string-join (map description1 cmds) "\n"))

(define (explain cmds)
  (display (description2 cmds)))

; json expressions

(define last-index 0)
(define (new-id)
  (set! last-index (add1 last-index))
   (number->string last-index))

; POINT
(define (json-point a b)
  (define identificador (new-id))
  (hash 'hidden #f 'type  "point" 'x a 'y b 'parents (hash) 'color "#c74440" 'id identificador))
  ;(define jsonexpression (jsexpr->string expression))
  ;(format jsonexpression))

; LINE
(define (json-line p1 p2)
  (define identificador (new-id))
  (hash 'hidden #f 'type  "line" 'parents (hash 'point1 p1 'point2 p2) 'color "#2d70b3" 'id identificador))

; CIRCLE
(define (json-circle p1 p2)
  (define identificador (new-id))
  (hash 'hidden #f 'type  "circle" 'parents (hash 'point1 p1 'point2 p2) 'color "#388c46" 'id identificador))

; INTERSECTION
(define (json-intersection obj1 obj2)
  (define identificador (new-id))
  (hash 'hidden #f 'type  "intersection" 'negRoot #f 'parents (hash 'host1 obj1 'host2 obj2) 'color "#c74440" 'id identificador))

(define (get-id obj)
  (string->symbol (hash-ref obj 'id)))


(define (elements objs)
  (for/hash ([o objs])
    (values (get-id o) o)))

; petit exemple
(define f (json-point 2 3))
(define g (json-point 1 9))
(define h (json-line f g))
(define example2 (list f g h))
;; (jsexpr->string (elements example2))
