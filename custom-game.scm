(define (game-custom-scenario)
  (define countt 0)
  (set! player 'W)
  (define horiz-inset 100)
  (define vert-inset 40)
  (set! board (list (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())
                    (list `() `() `() `() `() `() `() `())))
  (set! board1 board)
  (define v-random-board (open-viewport "Custom Scenario" 800 700))
  (define (db i j)
    (if (= i 9) (set! trash 0)
        (if (= j 9) (db (+ i 1) 1)
            (begin
              ((draw-pixmap v-random-board) (if (even? (+ i j)) "Images/white.png" "Images/black.png")
                                            (make-posn (+ horiz-inset (* 75 (- i 1))) (+ vert-inset (* 75 (- j 1))))
                                            (make-rgb 0 0 0))
              (db i (+ j 1))))))
  
  (db 1 1)
  ((draw-pixmap v-random-board) "Images/black-side-.png" (make-posn 337 5) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-pawn.png" (make-posn 12.5 95) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-king.png" (make-posn 12.5 170) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-queen.png" (make-posn 12.5 245) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-rook.png" (make-posn 12.5 320) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-bishop.png" (make-posn 12.5 395) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black-W-knight.png" (make-posn 12.5 470) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/black.png" (make-posn 12.5 545) (make-rgb 0 0 0))
  
  ((draw-pixmap v-random-board) "Images/white-side-.png" (make-posn 337 645) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-pawn.png" (make-posn 712.5 95) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-king.png" (make-posn 712.5 170) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-queen.png" (make-posn 712.5 245) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-rook.png" (make-posn 712.5 320) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-bishop.png" (make-posn 712.5 395) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white-B-knight.png" (make-posn 712.5 470) (make-rgb 0 0 0))
  ((draw-pixmap v-random-board) "Images/white.png" (make-posn 712.5 545) (make-rgb 0 0 0))
  
  ((draw-pixmap v-random-board) "Images/done-.png" (make-posn 550 665) (make-rgb 0 0 0))
  
  (define Bking 0)
  (define Bpawn 0)
  (define Brest 0)
  (define Wking 0)
  (define Wpawn 0)
  (define Wrest 0)
  (define (check-valid-board)
    (define bord (append (list-ref board 0) (list-ref board 1) (list-ref board 2) (list-ref board 3) (list-ref board 4) (list-ref board 5) (list-ref board 6) (list-ref board 7)))
    (define (count-no b)
      (cond ((null? b) (set! trash 0))
            ((equal? (car b) (cons 'B 'king)) (begin (set! Bking (+ Bking 1)) (count-no (cdr b))))
            ((equal? (car b) (cons 'W 'king)) (begin (set! Wking (+ Wking 1)) (count-no (cdr b))))
            ((equal? (car b) (cons 'B 'pawn)) (begin (set! Bpawn (+ Bpawn 1))
                                                     (set! Brest (+ Brest 1))
                                                     (count-no (cdr b))))
            ((equal? (car b) (cons 'W 'pawn)) (begin (set! Wpawn (+ Wpawn 1))
                                                     (set! Wrest (+ Wrest 1))
                                                     (count-no (cdr b))))
            ((null? (car b)) (count-no (cdr b)))
            (else (if (equal? (caar b) 'B)
                      (begin (set! Brest (+ Brest 1)) (count-no (cdr b)))
                      (begin (set! Wrest (+ Wrest 1)) (count-no (cdr b)))))))
    (count-no bord)
    (and
     (= Bking 1)
     (= Wking 1)
     (< Bpawn 9)
     (< Wpawn 9)
     (< Wrest 16)
     (< Brest 16)))
  
  
  (define (board-setter x1 y1 x2 y2)
    (define piece 'na)
    (define (piece-set)
      (if (< x1 100)
          (cond ((< y1 170) (set! piece (cons 'W 'pawn)))
                ((< y1 245) (set! piece (cons 'W 'king)))
                ((< y1 320) (set! piece (cons 'W 'queen)))
                ((< y1 395) (set! piece (cons 'W 'rook)))
                ((< y1 470) (set! piece (cons 'W 'bishop)))
                ((< y1 545) (set! piece (cons 'W 'knight)))
                (else (set! piece '())))
          (cond ((< y1 170) (set! piece (cons 'B 'pawn)))
                ((< y1 245) (set! piece (cons 'B 'king)))
                ((< y1 320) (set! piece (cons 'B 'queen)))
                ((< y1 395) (set! piece (cons 'B 'rook)))
                ((< y1 470) (set! piece (cons 'B 'bishop)))
                ((< y1 545) (set! piece (cons 'B 'knight)))
                (else (set! piece '())))))
    (piece-set)
    (define (update-line py line)
      (if (null? line) `()
          (if (= py 0) (cons piece (cdr line))
              (cons (car line) (update-line (- py 1) (cdr line))))))
    (define (update-board px brd)
      (if (null? brd) `()
          (if (= px 0) (cons (update-line (- x2 1) (car brd)) (cdr brd))
              (cons (car brd) (update-board (- px 1) (cdr brd))))))
    (set! board (update-board (- y2 1) board))
    ((draw-pixmap v-random-board) 
     (if (null? piece) (if (even? (+ x2 y2)) "Images/white.png" "Images/black.png")
         (string-append (if (even? (+ x2 y2)) "Images/white-" "Images/black-") (symbol->string (car piece)) "-" (symbol->string (cdr piece)) ".png"))
     (make-posn (+ horiz-inset (* (- x2 1) 75)) (+ vert-inset (* (- y2 1) 75))) (make-rgb 0 0 0))
    board)
  
  (define (scenario-make)
    (define pos (mouse-click-posn (get-mouse-click v-random-board)))
    (define x (posn-x pos))
    (define y (posn-y pos))
    (define (get-pos)
      (define pos (mouse-click-posn (get-mouse-click v-random-board)))
      (define x1 (+ 1 (quotient (- (posn-x pos) horiz-inset) 75)))
      (define y1 (+ 1 (quotient (- (posn-y pos) vert-inset) 75)))
      (cond ((and (< x1 9) (> x1 0) (< y1 9) (> y1 0)) (begin (set! board (board-setter x y x1 y1)) (scenario-make)))
            (else (scenario-make))))
    
    (cond
      ((and (> x 12.5) (< x 87.5) (> y 95) (< y 620)) (get-pos))
      ((and (> x 712.5) (< x 787.5) (> y 95) (< y 620)) (get-pos))
      ((and (> x 550) (< x 675) (> y 655) (< y 685)) (begin (set! board1 board)
                                                            (if (check-valid-board)
                                                                (close-viewport v-random-board)
                                                                (begin
                                                                  (if (even? countt)
                                                                      ((draw-pixmap v-random-board) "Images/invalid-board-1.png" (make-posn 0 0) (make-rgb 0 0 0))
                                                                      ((draw-pixmap v-random-board) "Images/invalid-board-2.png" (make-posn 0 0) (make-rgb 0 0 0)))
                                                                  (set! countt (+ countt 1))
                                                                  (set! Bking 0)
                                                                  (set! Bpawn 0)
                                                                  (set! Brest 0)
                                                                  (set! Wking 0)
                                                                  (set! Wpawn 0)
                                                                  (set! Wrest 0)
                                                                  (scenario-make)))))
      (else (scenario-make))))
  (scenario-make))
