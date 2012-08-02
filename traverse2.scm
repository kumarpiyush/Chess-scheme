(define (other-player player)
  (if (equal? player 'B) 'W 'B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (permut lst len)
  (define lst2 (generate-random-no-lst len `()))
  (foldr (lambda (x y) (cons (list-ref lst x) y)) `() lst2))
(define (generate-random-no-lst len mylst)
  (define randno (random len))
  (define ans (foldr (lambda (x y) (and (if (= x randno) #f #t) y)) #t mylst))
  (if (eq? (length mylst) len) mylst (if ans (generate-random-no-lst len (cons randno mylst)) (generate-random-no-lst len mylst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "rating.scm")

(define (alpha-beta given-board given-depth given-player)
  (define stored-board board)
  
  (define my-move 1)
  
  (define trash 0)
  
  (define (helper board depth alpha beta prev-alpha prev-beta  player)
    
    (define (for-loop1 lst)
      (if (null? lst) (set! trash 0)
          (begin
            (set! alpha  (max alpha (helper (update (caar lst) (cadar lst) board) (- depth 1) alpha beta prev-alpha prev-beta (other-player player))))
            (if (>= alpha beta) (set! trash 0)
                (begin
                  (cond 
                    ((and (not (equal? prev-alpha alpha)) (= depth given-depth)) (begin
                                                                                   (set! my-move (car lst))
                                                                                   (set! prev-alpha alpha))))
                  (for-loop1 (cdr lst)))))))
    
    
    (define (for-loop2 lst)
      (if (null? lst) (set! trash 0)
          (begin
            (set! beta (min beta (helper (update (caar lst) (cadar lst) board)  (- depth 1) alpha beta prev-alpha prev-beta  (other-player player))))
            (if (>= alpha beta) (set! trash 0)
                (begin
                  (cond 
                    ((and (not (equal? prev-beta beta)) (= depth given-depth)) (begin
                                                                                 (set! my-move (car lst))
                                                                                 (set! prev-beta beta))))
                  (for-loop2 (cdr lst)))))))
    
    
    (define list-of-all-possible-moves (all-possible-moves board player))
    (define list-of-all-possible-moves1 list-of-all-possible-moves)
    
    ;(cond ((and (> (length list-of-all-possible-moves) 1) (< (length list-of-all-possible-moves) 15))
    ;       (begin 
    ;         (set! list-of-all-possible-moves (permut list-of-all-possible-moves (length list-of-all-possible-moves))))))
             ;(if (equal? list-of-all-possible-moves list-of-all-possible-moves1) (display "FART  ") (display "YES!  ")))))
          
          
    ;(display list-of-all-possible-moves) (display #\newline)
    (cond
      ((and (null? list-of-all-possible-moves) (equal? player given-player))
       (cond ((not (in-check? given-player)) 
              (- (- (rate board) 32700067) (* 1000 depth)))
             (else (begin (define value (rate board)) (if (< value 0) (- value 1000) (+ value 1000))))))
      
      ((and (null? list-of-all-possible-moves) (not (equal? player given-player)))
       (cond ((not (in-check? given-player))
              (+ (+ 32700067 (rate board)) (* 1000 depth)))
             (else (begin (define value (rate board)) (if (< value 0) (- value 1000) (+ value 1000))))))
      ((= depth 0) (rate board))
      ((equal? player given-player) (begin 
                       (for-loop1 list-of-all-possible-moves)  ;(gnode-lst tree))
                       alpha))
      (else (begin
              (for-loop2 list-of-all-possible-moves);(gnode-lst tree))
              beta))))
  
  (begin
    (helper given-board given-depth -100000000000 100000000000 -100000000000 100000000000 'B)
    (set! board stored-board)
    (set! board1 stored-board)
    my-move))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (all-possible-moves given-board player)
  (define (iter i j)
    (cond ((= i 0) `())
          ((= j 0) (iter (- i 1) 8))
          ((or (null? (list-ref (list-ref given-board (- i 1)) (- j 1))) (not (eq? player (car (list-ref (list-ref given-board (- i 1)) (- j 1)))))) (iter i (- j 1)))
          ((null? (possible-moves (cons i j) given-board)) (iter i (- j 1)))
          (else (append (foldr (lambda (x y) (cons (list (cons i j) x) y)) '() (possible-moves (cons i j) given-board)) (iter i (- j 1))))))
  (iter 8 8))

(define (possible-moves pos given-board)
  (define (generate-list fr fc) ; fr and fc are functions on r and c resp.
    (define (gl count l)
      (define posr (fr count))
      (define posc (fc count))
      
      (if (and (> posr 0) (< posr 9) (> posc 0) (< posc 9)) (gl (+ 1 count) (append l (list (cons posr posc))))
          l))
    (gl 1 '()))
  
  (define r (car pos))
  (define c (cdr pos))
  (define piece (cdr (list-ref (list-ref given-board (- r 1)) (- c 1))))
  (define color (car (list-ref (list-ref given-board (- r 1)) (- c 1))))
  (define moves-list '())
  
  (set! moves-list
        (cond
          ((equal? piece `pawn) (if (equal? color `W) (list
                                                       (cons (- r 1) c)
                                                       (cons (- r 2) c)
                                                       (cons (- r 1) (- c 1))
                                                       (cons (- r 1) (+ c 1)))
                                    (list
                                     (cons (+ r 1) c)
                                     (cons (+ r 2) c)
                                     (cons (+ r 1) (- c 1))
                                     (cons (+ r 1) (+ c 1)))))
          
          ((equal? piece `rook) (append (generate-list (lambda (x) x) (lambda (y) c))
                                        (generate-list (lambda (x) r) (lambda (y) y))))
          
          ((equal? piece `knight) (list (cons (- r 2) (+ c 1)) (cons (- r 2) (- c 1))
                                        (cons (+ r 2) (+ c 1)) (cons (+ r 2) (- c 1))
                                        (cons (- r 1) (+ c 2)) (cons (- r 1) (- c 2))
                                        (cons (+ r 1) (+ c 2)) (cons (+ r 1) (- c 2))))
          
          ((equal? piece `bishop) (append
                                   (if (> r c) (generate-list (lambda (x) (- 9 x)) (lambda (y) (- (+ c 9) (+ r y)))) ; -ve slope line
                                       (generate-list (lambda (x) (- (+ r 9) (+ c x))) (lambda (y) (- 9 y))))
                                   
                                   (if (> (+ r c) 9) (generate-list (lambda (x) (- 9 x)) (lambda (y) (+ r c y -9)))
                                       (generate-list (lambda (x) (- (+ r c) x)) (lambda (y) y)))))
          
          ((equal? piece `queen) (append
                                  (append (generate-list (lambda (x) x) (lambda (y) c))
                                          (generate-list (lambda (x) r) (lambda (y) y)))
                                  (append
                                   (if (> r c) (generate-list (lambda (x) (- 9 x)) (lambda (y) (- (+ c 9) (+ r y)))) ; -ve slope line
                                       (generate-list (lambda (x) (- (+ r 9) (+ c x))) (lambda (y) (- 9 y))))
                                   
                                   (if (> (+ r c) 9) (generate-list (lambda (x) (- 9 x)) (lambda (y) (+ r c y -9)))
                                       (generate-list (lambda (x) (- (+ r c) x)) (lambda (y) y))))))
          ((equal? piece `king) (append (list (cons (+ r 1) (+ c 1)) (cons (+ r 1) c) (cons (+ r 1) (- c 1))
                                              (cons r (+ c 1)) (cons r (- c 1))
                                              (cons (- r 1) (+ c 1)) (cons (- r 1) c) (cons (- r 1) (- c 1)))
                                        (if (equal? color `W) (list (cons 8 7) (cons 8 3))
                                            (list (cons 1 7) (cons 1 3)))))
          (else "fart")))
  
  (foldr (lambda (x y) (if (final-check? pos x given-board color 'not) (cons x y)
                           y))
         '()
         moves-list))


(define aaaaaaaaaaaaa (cons 1 (cons 1 `())))