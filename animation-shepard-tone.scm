(import html)
(import canvas)
(import music)

(define canva (canvas 500 500))
canva

(rectangle canva 0 0 500 500 "solid" "white")

(define clicks (vector 0))

;;; (sqr) -> canvas image?
;;; outputs a square based on the number of clicks
(define sqr 
    (lambda ()
        (let 
            ([cl (vector-ref clicks 0) ])
            (rectangle canva (+ 0 (* 10 (- cl 1))) (- 500 (* 10 cl)) 10 10 "solid" "blue"))))

;;; (define trigger-function) -> void
;;; when called increases the click count and draws a new square
(define trigger-function 
    (lambda ()
        (begin 
            (vector-set! clicks 0 (+ 1 (vector-ref clicks 0)))
            (sqr))))


(canvas-onclick canva 
    (lambda (x y)
        (trigger-function)))

(animate-with 
    (lambda (time)
        (begin 
            (text canva "Shepard Tone Visualization" 125 50 "solid" "purple" "24px sans-serif"))))


