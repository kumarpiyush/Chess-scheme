(define (rate brd)
  (define tempboard board)
  (define tempboard1 board1)
  (set! board brd)
  (define tempplayer player)
  
  (define color `B)  ;; because player for AI is kept black
  (define othercolor `W)
  (define (points piece)
    (cond
      ((equal? piece `queen) 975)
      ((equal? piece `bishop) 325)
      ((equal? piece `knight) 320)
      ((equal? piece `rook) 500)
      ((equal? piece `pawn) 100)
      ((equal? piece `king) 32767)
      (else "give correct piece")))
  
  (define (kill-points piece)
    (cond
      ((equal? piece `queen) 15)
      ((equal? piece `bishop) 8)
      ((equal? piece `knight) 5)
      ((equal? piece `rook) 5)
      ((equal? piece `pawn) 2)
      ((equal? piece `king) 1000000000)
      (else "give correct piece")))
  
  (define grades 0) ; score of the board, will be updated
  
  (define (stage1)  ; points of players on the board
    (define r 1)
    (define c 1)
    
    (define (next-index)
      (if (= 8 c) (begin
                    (set! c 1)
                    (set! r (+ 1 r)))
          (set! c (+ 1 c))))
    
    (define (s1)
      (define place (if (<= r 8) (list-ref (list-ref brd (- r 1)) (- c 1))
                        '()))
      (cond
        ((and (= 9 r) (= c 1)) #t)
        ((null? place) (begin
                         (next-index)
                         (s1)))
        ((equal? color (car place)) (begin
                                      (set! grades (+ grades (points (cdr place))))
                                      (next-index)
                                      (s1)))
        (else (begin
                (set! grades (- grades (points (cdr place))))
                (next-index)
                (s1)))))
    (s1))
  
  (define (stage2)
    (define r 1)
    (define c 1)
    (define (next-index)
      (if (= 8 c) (begin
                    (set! c 1)
                    (set! r (+ 1 r)))
          (set! c (+ 1 c))))
    
    (define (eval-kill-pts l)
      (cond
        ((null? l) 0)
        ((null? (list-ref (list-ref brd (- (car (car l)) 1)) (- (cdr (car l)) 1))) (eval-kill-pts (cdr l)))
        (else (+ (kill-points (cdr (list-ref (list-ref brd (- (car (car l)) 1)) (- (cdr (car l)) 1)))) (eval-kill-pts (cdr l))))))
    
    ;    (define (valid-moves pos)
    ;      (define (moves-for-one-piece pos1 i j)
    ;        (if (= i 9) (begin (set! board1 board)'())
    ;            (if (= j 9) (moves-for-one-piece pos1 (+ 1 i) 1)
    ;                (if (final-check? pos1 (cons i j) 'not) (begin (set! board1 board) (append (list (cons i j)) (moves-for-one-piece pos1 i (+ j 1))))
    ;                    (begin (set! board1 board) (moves-for-one-piece pos1 i (+ j 1)))))))
    ;      (moves-for-one-piece pos 1 1))
    
    (define (s2)
      (define place (if (<= r 8) (list-ref (list-ref brd (- r 1)) (- c 1)) '()))
      (cond
        ((and (= 9 r) (= c 1)) #t)
        ((null? place) (begin
                         (next-index)
                         (s2)))
        ((equal? color (car place)) (begin
                                      (set! player color)
                                      (set! grades (+ grades (eval-kill-pts (valid-moves (cons r c)))))
                                      (next-index)
                                      (s2)))
        (else (begin
                (set! player othercolor)
                (set! grades (- grades (eval-kill-pts (valid-moves (cons r c)))))
                (next-index)
                (s2)))))
    
    (s2))
  (begin
    (stage1)
    ;(stage2)
    (set! board1 tempboard1)
    (set! board tempboard)
    (set! player tempplayer)
    grades))