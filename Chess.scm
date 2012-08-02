;#lang racket
(include "definitions.scm")

(define (detect-button-click image-center)
  (define pos (mouse-click-posn (get-mouse-click my-window)))
  
  (cond ((equal? 'no-useful-click (send image-center search-for-img pos)) (detect-button-click image-center))))

(define (call2) (begin
                  (make-object input-box "Black" (lambda (x) (begin
                                                                 (set! name2 x)
                                                                 (play))))
                  (set! trash 0)))

(define (call4) (begin
                  (make-object input-box "Black" (lambda (x) (begin
                                                                 (set! name2 x)
                                                                 (call3))))
                  (set! trash 0)))



(define (call3) (begin
                  
                  ;(display "call3")
                  (game-custom-scenario)
                  (count-deads-frm-brd board)
                  (play)))

(define my-window (open-viewport "Chess" (+ right-gap horiz-inset breadth) (+ bottom-gap vert-inset length1)))
((draw-pixmap my-window) "Images/Chess.jpg" (make-posn 0 0) (make-rgb 0 0 0))
(sleep 2)
((clear-viewport my-window))
  


(define (main)
  ((draw-pixmap my-window) "Images/win2.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  ((draw-pixmap my-window) "Images/Chess.png" (make-posn 330 30) (make-rgb 0 0 0))
  (define main-image-center (make-object img-center%))
  (define new-game (make-object image-button% "Images/new-game.png" 250 175 160 40 (lambda() 
                                                                               (begin
                                                                                (sleep 0.3)
                                                                                (set! board ideal-board)
                                                                                (set! player 'W)
                                                                                (set! board1 board)
                                                                                (set! vector-dead-black (vector 0 0 0 0 0))
                                                                                (set! vector-dead-white (vector 0 0 0 0 0))
                                                                                ((clear-viewport my-window))
                                                                               (func-new-game)))  main-image-center))
  (define load-game (make-object image-button% "Images/load-game.png"  590 175 160 40 (lambda() 
                                                                                  (begin
                                                                                    (sleep 0.3)
                                                                                    ((clear-viewport my-window))
                                                                                    (load-win)))  main-image-center))
  (define custom-game (make-object image-button% "Images/custom-game.png" 250 245 160 40 (lambda() 
                                                                                     (begin
                                                                                       (sleep 0.3)
                                                                                       ((clear-viewport my-window))
                                                                                       (func-custom-game))) main-image-center))
  (define instructions (make-object image-button% "Images/instructions.png" 590 245 160 40 (lambda() 
                                                                                       (begin
                                                                                         (sleep 0.3)
                                                                                         ((clear-viewport my-window))
                                                                                         (instructions1))) main-image-center))
  (define credits (make-object image-button% "Images/credits.png" 250 315 160 40 (lambda() 
                                                                             (begin
                                                                               (sleep 0.3)
                                                                               ((clear-viewport my-window))
                                                                               (func-credits))) main-image-center))
  (define quit (make-object image-button% "Images/exit.png" 590 315 160 40 (lambda() 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         (close-graphics) #t))  main-image-center))
  (begin
    (send new-game draw)
    (send load-game draw)
    (send custom-game draw)
    (send instructions draw)
    (send credits draw)
    (send quit draw)
    (detect-button-click main-image-center)))

(define (func-new-game)       
  ;(display "new-game")
  ((draw-pixmap my-window) "Images/new-game.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  ((draw-pixmap my-window) "Images/1-player.png" (make-posn 420 200) (make-rgb 0 0 0))
  (define new-game-center (make-object img-center%))
  (define level-1 (make-object image-button% "Images/level-1.png" 270 250 120 30 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 2)
                                                                                 (set! name2 "Computer")
                                                                                 (make-object input-box "Your name..." (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                  (play)))))) new-game-center))
  (define level-2 (make-object image-button% "Images/level-2.png" 440 250 120 30 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 3)
                                                                                 (set! name2 "Computer")
                                                                                 (make-object input-box "Your name..." (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                  (play)))))) new-game-center))
  (define level-3 (make-object image-button% "Images/level-3.png" 610 250 120 30 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 4)
                                                                                 (set! name1 "Computer")
                                                                                 (make-object input-box "Your name..." (lambda (x) (begin
                                                                                                                                  (set! name2 x)
                                                                                                                                  (play)))))) new-game-center))
  (define 2-player (make-object image-button% "Images/2-player.png" 420 320 160 40 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '2-player)
                                                                                 (make-object input-box "White" (lambda (x) (begin
                                                                                                                                (set! name1 x)
                                                                                                                                 (call2)))))) new-game-center))
  (define back (make-object image-button% "Images/back.png" 420 420 160 40 (lambda () 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (main))) new-game-center))
  
  (begin
    (send level-1 draw)
    (send level-2 draw)
    (send level-3 draw)
    (send 2-player draw)
    (send back draw)
    (detect-button-click new-game-center)))



(define (func-custom-game)
  ;(display "enter")
  ((draw-pixmap my-window) "Images/new-game.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  ((draw-pixmap my-window) "Images/1-player.png" (make-posn 420 200) (make-rgb 0 0 0))
  (define custom-game-center (make-object img-center%))
  (define level-1-custom (make-object image-button% "Images/level-1.png" 270 250 160 40 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 2)
                                                                                 (set! name2 "Computer")
                                                                                 ;(display "here")
                                                                                 (make-object input-box "Your name" (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                  (define (loop)
                                                                                                                                    (cond
                                                                                                                                      ((equal? name1 "") (loop))
                                                                                                                                      (else (call3))))
                                                                                                                                  (loop))))))
                            custom-game-center))
  (define level-2-custom (make-object image-button% "Images/level-2.png" 440 250 160 40 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 3)
                                                                                 (set! name2 "Computer")
                                                                                 ;(display "here")
                                                                                 (make-object input-box "Your name" (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                  (define (loop)
                                                                                                                                    (cond
                                                                                                                                      ((equal? name1 "") (loop))
                                                                                                                                      (else (call3))))
                                                                                                                                  (loop))))))
                            custom-game-center))
  (define level-3-custom (make-object image-button% "Images/level-3.png" 610 250 160 40 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '1-player)
                                                                                 (set! depth 4)
                                                                                 (set! name2 "Computer")
                                                                                 ;(display "here")
                                                                                 (make-object input-box "Your name" (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                  (define (loop)
                                                                                                                                    (cond
                                                                                                                                      ((equal? name1 "") (loop))
                                                                                                                                      (else (call3))))
                                                                                                                                  (loop))))))
                            custom-game-center))
  
  (define 2-player-custom (make-object image-button% "Images/2-player.png" 420 320 160 40 (lambda () 
                                                                               (begin
                                                                                 (sleep 0.3)
                                                                                 ((clear-viewport my-window))
                                                                                 (set! mode '2-player)
                                                                                 (make-object input-box "White" (lambda (x) (begin
                                                                                                                                  (set! name1 x)
                                                                                                                                 (call4))))))  custom-game-center))

  (define back (make-object image-button% "Images/back.png" 420 420 160 40 (lambda () 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (main))) custom-game-center))
  (begin
    ;(display "custom")
    (send level-1-custom draw)
    (send level-2-custom draw)
    (send level-3-custom draw)
    (send 2-player-custom draw)
    (send back draw)
    (detect-button-click custom-game-center)))

(define (instructions1)
  ((draw-pixmap my-window) "Images/Instructions1.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  (define instructions1-center (make-object img-center%))
  (define next (make-object image-button% "Images/next.png" 300 630 160 40 (lambda () 
                                                                       (begin
                                                                         ;(sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (instructions2))) instructions1-center))
  (define back (make-object image-button% "Images/back.png" 500 630 160 40 (lambda () 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (main))) instructions1-center))
  (begin
    (send next draw)
    (send back draw)
    (detect-button-click instructions1-center)))

(define (func-credits)
  ((draw-pixmap my-window) "Images/Credits.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  (define credits-center (make-object img-center%))
  (define back (make-object image-button% "Images/back.png" 420 630 160 40 (lambda () 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (main))) credits-center))
  (begin
    (send back draw)
    (detect-button-click credits-center)))

(define (instructions2)
  ((draw-pixmap my-window) "Images/Instructions2.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  (define instructions2-center (make-object img-center%))
  (define back (make-object image-button% "Images/back.png" 420 630 160 40 (lambda () 
                                                                       (begin
                                                                         (sleep 0.3)
                                                                         ((clear-viewport my-window))
                                                                         (instructions1))) instructions2-center))
  (begin
    (send back draw)
    (detect-button-click instructions2-center)))

(main)