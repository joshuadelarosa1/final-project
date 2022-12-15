(import html)
(import canvas)
(import music)

(define canva (canvas 500 500))
canva

(rectangle canva 0 0 500 500 "solid" "white")

(define clicks (vector 0))
(define clicksTwo (vector 0))

;;; (sqr) -> canvas image?
;;; row -> integer, 1 2 or 3
;;; outputs squarex based on the number of clicks 
(define sqr 
    (lambda (row)
        (let 
            ([cl (vector-ref clicks 0)]
            [cl2 (vector-ref clicksTwo 0)])
            (cond
                [(= row 1) (rectangle canva (+ 0 (* 5 (- cl 1)))(- 292 (* 8 cl2)) 10 10 "solid" "red")]
                [(= row 2) (rectangle canva (+ 0 (* 5 (- cl 1)))(- 396 (* 8 cl2)) 10 10 "solid" "green")]
                [(= row 3)(rectangle canva (+ 0 (* 5 (- cl 1)))(- 500 (* 8 cl2)) 10 10 "solid" "blue")]))))

;;; (define sqrs) -> outputs canvas images
;;; draws squares and resets at 14 clicks
(define sqrs
    (lambda ()
        (let 
            ([cl (vector-ref clicksTwo 0)])
            (cond 
                [(< cl 14)
                    (begin
                        (sqr 1)
                        (sqr 2)
                        (sqr 3))]
                [else 
                    (begin 
                        (vector-set! clicksTwo 0 0)
                        (vector-set! clicks 0 (- (vector-ref clicks 0) 1)))]))))


;;; (define trigger-function) -> void
;;; when called increases the click count and draws a new squares
(define trigger-function 
    (lambda ()
        (begin 
            (vector-set! clicks 0 (+ 1 (vector-ref clicks 0)))
            (vector-set! clicksTwo 0 (+ 1 (vector-ref clicksTwo 0)))
            (sqrs))))

(canvas-onclick canva 
    (lambda (x y)
        (trigger-function)))

(animate-with 
    (lambda (time)
        (begin 
            (text canva "Rissett Rhythm Visualization" 125 50 "solid" "purple" "24px sans-serif"))))