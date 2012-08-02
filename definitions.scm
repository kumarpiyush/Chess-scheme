(require graphics/graphics)
(open-graphics)
(require racket/list)
(include "file.scm")
(include "input.scm")
(include "update.scm")
(include "validation.scm")
(include "custom-game.scm")
(include "traverse2.scm")
(include "play.scm")
(define horiz-inset 200)
(define vert-inset 35)
(define right-gap 200)
(define bottom-gap 50)
(define horiz 8)
(define vert 8)
(define img-breadth 75)
(define img-length 75)
(define breadth (* horiz img-breadth))
(define length1 (* vert img-length))
(define possible-moves-color (make-rgb (/ 93 255) (/ 237 255) (/ 71 255)))
(define select-color (make-rgb 1 0 0))
(define mode '1-player)
(define depth 3)
(define name1 "")
(define name2 "")

(define image-button%
  (class object%
    (init-field name)
    (init-field horiz)
    (init-field vert)
    (init-field len)
    (init-field breadth)
    (init-field action)
    (init-field img-center)
    (super-new)
    (define/public (draw)
      ((draw-pixmap my-window) name (make-posn horiz vert) (make-rgb 0 0 0))
      (send img-center add this))
    
    (define/public (is-inside? pos)
      (define x (posn-x pos))
      (define y (posn-y pos))
      (cond
        ((and (>= x horiz) (<= x (+ horiz len)) (>= y vert) (<= y (+ vert breadth))) #t)
        (else #f)))
    
    (define/public (perform-action) (action))))

(define img-center%
  (class object%
    [field (lst '())]
    (super-new)
    
    (define/public (add img-name)
      (set! lst (append (list img-name) lst)))
    
    (define/public (search-for-img pos)
      (define (sfi l)
        (cond
          ((null? l) 'no-useful-click)
          (else (begin
                  (define ret (send (car l) is-inside? pos))
                  (if (equal? ret #t) (send (car l) perform-action)
                      (sfi (cdr l)))))))
                      
        (sfi lst))))