(require racket/gui)
(define input-box
  (class object%
    [init-field title]
    [init-field proc-ok]
    [super-new]
    (define input (new frame% 
                       [label title]
                       [min-width 200]
                       [min-height 75]
                       [x 400]
                       [y 50]
                       [stretchable-width #f]
                       [stretchable-height #f]))
    
    (define inp-box (new text-field% 
                         [label #f]
                         [parent input]
                         [vert-margin 10]
                         [init-value ""]
                         [enabled #t]
                         [min-width 200]
                         [stretchable-width #f]
                         [stretchable-height #f]))
    
    
    (define buttons (new horizontal-panel% 
                         [parent input]
                         [alignment '(left top)]))
    
    (define ok-button (new button%
                           [label "OK"]
                           [parent buttons]
                           [min-width 50]
                           [min-height 20]
                           [vert-margin 0]
                           [horiz-margin 100]
                           [callback (lambda (button event)
                                       (begin
                                         (send input show #f)
                                         (proc-ok (send inp-box get-value))
                                         ))]))
    
;    (define cancel-button (new button%
;                               [label "Cancel"]
;                               [parent buttons]
;                               [min-width 50]
;                               [min-height 20]
;                               [callback (lambda (button event)
;                                           (send input show #f))]))
;    
    (send input show #t)))

