(define (display-players)
  (cond
    ((equal? player 'W) (begin
                          ((draw-solid-rectangle my-window) (make-posn 825 30) 150 250 (make-rgb 1 1 1))
                          ((draw-pixmap my-window) "Images/Large-white.png" (make-posn 25 350) (make-rgb 0 0 0))
                          ((draw-pixmap my-window) "Images/Small-black.png" (make-posn 870 105) (make-rgb 0 0 0))))
    (else (begin
            ((draw-solid-rectangle my-window) (make-posn 25 350) 150 250 (make-rgb 1 1 1))
            ((draw-pixmap my-window) "Images/Large-black.png" (make-posn 825 30) (make-rgb 0 0 0))
            ((draw-pixmap my-window) "Images/Small-white.png" (make-posn 70 425) (make-rgb 0 0 0))))))

(define vector-dead-black (vector 0 0 0 0 0))    ;; rook , knight , bishop , queen , pawn
(define vector-dead-white (vector 0 0 0 0 0))    ;; rook , knight , bishop , queen , pawn


(define (count-deads-frm-brd brd)
  (define vector-live-black (vector 0 0 0 0 0))
  (define vector-live-white (vector 0 0 0 0 0))
  (define r 0)
  (define c 0)
  (define (next)
    (if (= c 7) (begin
                  (set! r (+ 1 r))
                  (set! c 0))
        (set! c (+ 1 c))))
  
  (define (loop)
    (cond
      ((and (= 8 r) (= c 0)) (begin
                               (set! vector-dead-white (vector
                                                        (max 0 (- 2 (vector-ref vector-live-white 0)))
                                                        (max 0 (- 2 (vector-ref vector-live-white 1)))
                                                        (max 0 (- 2 (vector-ref vector-live-white 2)))
                                                        (max 0 (- 1 (vector-ref vector-live-white 3)))
                                                        (max 0 (- 8 (vector-ref vector-live-white 4)))))
                               
                               (set! vector-dead-black (vector
                                                        (max 0 (- 2 (vector-ref vector-live-black 0)))
                                                        (max 0 (- 2 (vector-ref vector-live-black 1)))
                                                        (max 0 (- 2 (vector-ref vector-live-black 2)))
                                                        (max 0 (- 1 (vector-ref vector-live-black 3)))
                                                        (max 0 (- 8 (vector-ref vector-live-black 4)))))
                               #t))
      ((null? (list-ref (list-ref board r) c)) (begin (next) (loop)))
      (else (begin
              (define piece (list-ref (list-ref board r) c))
              (if (equal? `W (car piece)) (cond
                                            ((equal? `rook (cdr piece)) (vector-set! vector-live-white 0 (+ 1 (vector-ref vector-live-white 0))))
                                            ((equal? `knight (cdr piece)) (vector-set! vector-live-white 1 (+ 1 (vector-ref vector-live-white 1))))
                                            ((equal? `bishop (cdr piece)) (vector-set! vector-live-white 2 (+ 1 (vector-ref vector-live-white 2))))
                                            ((equal? `queen (cdr piece)) (vector-set! vector-live-white 3 (+ 1 (vector-ref vector-live-white 3))))
                                            ((equal? `pawn (cdr piece)) (vector-set! vector-live-white 4 (+ 1 (vector-ref vector-live-white 4)))))
                  (cond
                    ((equal? `rook (cdr piece)) (vector-set! vector-live-black 0 (+ 1 (vector-ref vector-live-black 0))))
                    ((equal? `knight (cdr piece)) (vector-set! vector-live-black 1 (+ 1 (vector-ref vector-live-black 1))))
                    ((equal? `bishop (cdr piece)) (vector-set! vector-live-black 2 (+ 1 (vector-ref vector-live-black 2))))
                    ((equal? `queen (cdr piece)) (vector-set! vector-live-black 3 (+ 1 (vector-ref vector-live-black 3))))
                    ((equal? `pawn (cdr piece)) (vector-set! vector-live-black 4 (+ 1 (vector-ref vector-live-black 4))))))
              (next)
              (loop)))))
  (loop))


(define (pos-W i)
  (cond
    ((= i 0) (make-posn 125 112))
    ((= i 1) (make-posn 125 162))
    ((= i 2) (make-posn 125 212))
    ((= i 3) (make-posn 125 262))
    ((= i 4) (make-posn 125 312))))

(define (pos-B i)
  (cond
    ((= i 0) (make-posn 925 312))
    ((= i 1) (make-posn 925 362))
    ((= i 2) (make-posn 925 412))
    ((= i 3) (make-posn 925 462))
    ((= i 4) (make-posn 925 512))))

(define (index-of piece)
  (cond
    ((equal? piece 'rook) 0)
    ((equal? piece 'knight) 1)
    ((equal? piece 'bishop) 2)
    ((equal? piece 'queen) 3)
    ((equal? piece 'pawn) 4)))

(define (update-deads pos2)
  (define sqr  (get-square (cons (cdr pos2) (car pos2))))
  
  (cond 
    ((not (null? sqr)) (begin
                         (define color (car sqr))
                         (define piece (cdr sqr))
                         (define vec (if (equal? color 'W) vector-dead-white vector-dead-black))
                         (define dead-numbers (vector-ref vec (index-of piece)))
                         (vector-set! vec (index-of piece) (+ 1 dead-numbers))))))


(define (change-counts)
  
  (define (helper-W i)
    (cond
      ((not (> i 4))
       (begin
         ((draw-pixmap my-window) (string-append "Images/" (number->string (vector-ref vector-dead-white i)) ".png") (pos-W i) (make-rgb 0 0 0))
         (helper-W (+ i 1))))))
  
  (define (helper-B i)
    (cond
      ((not (> i 4))
       (begin
         ((draw-pixmap my-window) (string-append "Images/" (number->string (vector-ref vector-dead-black i)) ".png") (pos-B i) (make-rgb 0 0 0))
         (helper-B (+ i 1))))))
  
  (begin (helper-W 0) (helper-B 0)))



(define (initialize)
  (begin
    ((draw-string my-window) (make-posn 860 25) name2 (make-rgb 0 0 0)) 
    ((draw-string my-window) (make-posn 70 650) name1 (make-rgb 0 0 0))
    (display-players)
    (change-counts)
    ((draw-pixmap my-window) "Images/new-game-play.png" (make-posn 270 645) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/save-game-play.png" (make-posn 570 645) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/W-rook.png" (make-posn 75 106) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/W-knight.png" (make-posn 75 156) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/W-bishop.png" (make-posn 75 206) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/W-queen.png" (make-posn 75 256) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/W-pawn.png" (make-posn 75 306) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/B-rook.png" (make-posn 875 306) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/B-knight.png" (make-posn 875 356) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/B-bishop.png" (make-posn 875 406) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/B-queen.png" (make-posn 875 456) (make-rgb 0 0 0))
    ((draw-pixmap my-window) "Images/B-pawn.png" (make-posn 875 506) (make-rgb 0 0 0))))





(define (draw-board)
  (define (draw-a-line given-y i)
    (define x  (+ 1 (/ (- i horiz-inset) img-breadth)))
    (define y  (+ 1 (/ (- given-y vert-inset) img-length)))
    (if (> (+ horiz-inset (- breadth img-breadth)) i) 
        (begin 
          ((draw-pixmap my-window) (give-image (cons x y) "") (make-posn i given-y) (make-rgb 0 0 0))
          (draw-a-line given-y (+ i img-breadth)))
        ((draw-pixmap my-window) (give-image (cons x y) "") (make-posn i given-y) (make-rgb 0 0 0))))
  (define (draw-lines j)
    (if (> (+ vert-inset (- length1 img-length)) j)
        (begin
          (draw-a-line j  horiz-inset)
          (draw-lines (+ j img-length)))
        (draw-a-line j horiz-inset)))
  (draw-lines vert-inset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (give-string square) ;; to take images of board
  (if (null? (get-square square)) (string-append "Images/" (symbol->string (background-color square)))
      (string-append "Images/" (symbol->string (background-color square)) "-" (symbol->string (car (get-square square))) "-" (symbol->string (cdr (get-square square))))))

(define (give-image square clause-string)
  (string-append (give-string square) clause-string ".png"))


(define (draw-image position clause) ;;here, position is a cons pair
  (define x (car position)) (define y (cdr position))
  ((draw-pixmap my-window) (give-image (cons x y) clause) (make-posn (+ horiz-inset (* img-breadth (- x 1))) (+ vert-inset (* img-length (- y 1)))) (make-rgb 0 0 0)))



(define count1 1)
(define square1 (cons 0 0))
(define square2 (cons 0 0))


(define (show-possible-moves square board)
  (define plist (possible-moves square board))
  (define (helper lst)
    (cond
      ((not (null? lst)) (begin
                           (define y (caar lst))
                           (define x (cdar lst))
                           (draw-image (cons x y) "-possible-move")
                           (helper (cdr lst))))))
  (helper plist))


(define (get-continue win) ;; continue after game end, depends on what to call
  (define pos (mouse-click-posn (get-mouse-click win)))
  (cond
    ((and (>= (posn-x pos) 170) (<= (posn-x pos) 330) (>= (posn-y pos) 300) (<= (posn-y pos) 340)) (begin
                                                                                                     (close-viewport win)
                                                                                                     (main)))
    (else (get-continue win))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (detect-square)
  (define pos (mouse-click-posn (get-mouse-click my-window)))
  (define x (+ 1 (quotient (- (posn-x pos) horiz-inset) img-breadth)))
  (define y (+ 1 (quotient (- (posn-y pos) vert-inset) img-length)))
  (cond
    ((and (>= (posn-x pos) 270) (<= (posn-x pos) 430) (>= (posn-y pos) 645) (<= (posn-y pos) 684)) (main))
    ((and (>= (posn-x pos) 570) (<= (posn-x pos) 730) (>= (posn-y pos) 645) (<= (posn-y pos) 684)) (save-win))
    ((and (= (remainder count1 2) 1) (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) (< x 9) (< y 9) (null? (get-square (cons x y)) ))
     (detect-square))
    ((and (= (remainder count1 2) 1)  (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) (< x 9) (< y 9) (not (equal? player (car (get-square (cons x y))))))
     (detect-square)) 
    ((and (= (remainder count1 2) 1)  (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) (< x 9) (< y 9) (equal? player (car (get-square (cons x y)))))
     (begin
       (set! square1 (cons y x))
       (draw-image (cons x y) "-selected")
       (show-possible-moves square1 board)
       (set! count1 (+ count1 1))
       (detect-square)))
    ((and (= (remainder count1 2) 0) (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) (< x 9) (< y 9) )
     (begin
       (set! square2 (cons y x))
       (set! count1 (+ count1 1))
       (if (and (not (equal? square2 square1)) (final-check? square1 square2 board player))
           (begin
             (update-deads square2) 
             (set! board board1)
             (draw-board)
             (change-counts)
             (change-player)
             (display-players)
             (cond ((in-check? player) (draw-image (cons (cdr kpos) (car kpos)) "-check")))
             (cond
               ((null? (all-valid-moves)) 
                (cond
                  ((and (equal? player 'B) (in-check? player)) (begin
                                                                 (define player-1-loses (open-viewport "Check-Mate" 500 375))
                                                                 ((draw-pixmap player-1-loses) "Images/Player1.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                                 ((draw-pixmap player-1-loses) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                                                                 (get-continue player-1-loses)))
                  ((and (equal? player 'W) (in-check? player)) (begin
                                                                 (define player-2-loses (open-viewport "Check-Mate" 500 375))
                                                                 ((draw-pixmap player-2-loses) "Images/Player2.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                                 ((draw-pixmap player-2-loses) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                                                                 (get-continue player-2-loses)))
                  (else (begin
                          (define stale-mate (open-viewport "Stale-Mate" 500 375))
                          ((draw-pixmap stale-mate) "Images/Stale-mate.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                          ((draw-pixmap stale-mate) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                          (get-continue stale-mate)))))
                 ((equal? mode '2-player) (detect-square))
                 ((equal? mode '1-player) (comp-play))))
           (begin
             (draw-board)
             (cond ((in-check? player) (draw-image (cons (cdr kpos) (car kpos)) "-check")))
             (detect-square)))))
     (else (detect-square))))
  


(define (comp-play)
  (define move (alpha-beta board depth 'B))
  (define piece (list-ref (list-ref board (- (car (car move)) 1)) (- (cdr (car move)) 1)))
  (cond ((and (equal? piece (cons 'B 'pawn)) (= 8 (car (car (cdr move))))) (change-board-for-pawn-upgrade 'queen (car move) (cadr move))))
  (begin
    (update-deads (cadr move)) 
    (set! board (update (car move) (cadr move) board))
    (set! board1 board)
    (draw-board)
    (change-counts)
    (change-player)
    (display-players)
    (cond ((in-check? player) (draw-image (cons (cdr kpos) (car kpos)) "-check")))
    (cond
      ((null? (all-valid-moves)) 
       (cond
         ((and (equal? player 'B) (in-check? player)) (begin
                                                        (define comp-loses (open-viewport "Check-Mate" 500 375))
                                                        ((draw-pixmap comp-loses) "Images/Computer.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                        ((draw-pixmap comp-loses) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                                                        (get-continue comp-loses)))
         ((and (equal? player 'W) (in-check? player)) (begin
                                                        (define you-lose (open-viewport "Check-Mate" 500 375))
                                                        ((draw-pixmap you-lose) "Images/You.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                        ((draw-pixmap you-lose) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                                                        (get-continue you-lose)))
         (else (begin
                 (define stale-mate (open-viewport "Stale-Mate" 500 375))
                 ((draw-pixmap stale-mate) "Images/Stale-mate.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                 ((draw-pixmap stale-mate) "Images/continue.png" (make-posn 170 300) (make-rgb 0 0 0))
                 (get-continue stale-mate)))))
       (else (detect-square)))))

(define (play)
  (initialize)
  (draw-board)
  (detect-square))



