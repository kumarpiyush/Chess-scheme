(define ideal-board
  (list (list (cons 'B 'rook) (cons 'B 'knight) (cons 'B 'bishop) (cons 'B 'queen) (cons 'B 'king) (cons 'B 'bishop) (cons 'B 'knight) (cons 'B 'rook))
        (list (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn)  (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn))
        (list '()            '()               '()               '()              '()             '()               '()               '())
        (list '()            '()               '()               '()              '()             '()               '()               '())
        (list '()            '()               '()               '()              '()             '()               '()               '())
        (list '()            '()               '()               '()              '()             '()               '()                '())
        (list (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn)  (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn))
        (list (cons 'W 'rook) (cons 'W 'knight) (cons 'W 'bishop) (cons 'W 'queen) (cons 'W 'king) (cons `W `bishop) (cons `W `knight) (cons 'W 'rook))))
(define board ideal-board)
(define board1 board)

(define player 'W)

(define (update-board given-board x y new-element)
  (define (list-update j new lst)       
    (if (= 1 j) (append (list new) (cdr lst))
        (append (list (car lst)) (list-update (- j 1) new (cdr lst)))))
  (list-update x (list-update y new-element (list-ref given-board (- x 1))) given-board))

(define (update original-position final-position . l)  ; l contains initial given board, also serves to return updated board
  
  (define original-x (car original-position))
  (define original-y (cdr original-position))
  (define final-x (car final-position))
  (define final-y (cdr final-position))
  
  (cond
    ((null? l) (begin (define piece (list-ref (list-ref board1 (- original-x 1))  (- original-y 1)))
                      (set! board1 (update-board (update-board board1 original-x original-y '()) final-x final-y piece))))
    (else (begin (define piece (list-ref (list-ref (car l) (- original-x 1))  (- original-y 1)))
                 (update-board (update-board (car l) original-x original-y '()) final-x final-y piece)))))


(define (get-square square)
  (list-ref (list-ref board (- (cdr square) 1)) (- (car square) 1)))

(define (change-player)
  (if (equal? player 'B) (set! player 'W)
      (set! player 'B)))

(define (background-color square)
  (if (even? (+ (car square) (cdr square))) 'white
      'black))

(define (get-position player piece)
  (define (search pair)
    (define (helper1 i)
      (define (helper2 x j)
        (cond
          ((= 9 j) '())
          ((equal? (get-square (cons j x)) pair) (cons x j))
          (else (helper2 x (+ j 1)))))
      (cond
        ((= 9 i) '())
        ((not (null? (helper2 i 1))) (helper2 i 1))
        (else (helper1 (+ i 1)))))
    (if (null? (helper1 1)) 'not-found
        (helper1 1)))
  (search (cons player piece)))