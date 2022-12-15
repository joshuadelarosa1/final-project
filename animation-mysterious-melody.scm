(import html)
(import canvas)
(import music)

(define canva (canvas 500 500))
canva

(rectangle canva 0 0 500 500 "solid" "white")

(define clicks (vector 0))

;;; (define rect) -> canvas image?
;;; x -> intege? valid coordinate
;;; y -> integer? valid coordinate
;;; draws rectangle at specified spot
(define rect 
    (lambda (x y) 
        (rectangle canva x y 5 400 "solid" "purple")))

;;; (define mock-note) -> canvas text? 
;;; x -> integer? valid coordiante
;;; y -> integer? valid coordinate
;;; draws mock-note at specified spot
(define mock-note 
    (lambda (x y)
        (begin 
            (text canva "__" x y "solid" "black" "12px sans-serif")
            (text canva "|--" x (+ y 10) "solid" "black" "12px sans-serif")
            (text canva "|" x (+ y 20) "solid" "black" "12px sans-serif")
            (text canva "()|" (- x 8) (+ y 30) "solid" "black" "12px sans-serif"))))

;;; (define mystery-note) -> canvas text?
;;; x -> integer? valid coordinate?
;;; y -> integer? valid coordiante? 
;;; draws mystery note at specified spot
(define mystery-note
    (lambda (x y) 
        (begin
            (text canva "?" x y "solid" "black" "24px sans-serif"))))

;;; (define example) ->  canvas text or image?
;;; based on the number of clicks outputs text or image at semi-randomized spot
(define example 
    (lambda () 
        (let 
            ([cl (vector-ref clicks 0)])
            (cond 
                [(< cl 5) 
                    (mystery-note (+ 20 (* cl 20)) (+ 100 (random 300)))]
                [(< cl 9)
                    (mock-note (+ 170 (* (- cl 4) 25)) (+ 150 (random 200)))]
                [(< cl 13)
                    (mock-note (+ 340 (* (- cl 8) 25)) (+ 150 (random 200)))]))))

;;; (define trigger-function) -> void
;;; increments vector and calls example
(define trigger-function 
    (lambda ()
        (begin 
            (vector-set! clicks 0 (+ 1 (vector-ref clicks 0)))
            (example))))

(canvas-onclick canva 
    (lambda (x y)
        (trigger-function)))

(animate-with 
    (lambda (time)
        (begin 
            (text canva "Mysterious Melody Visualization" 100 50 "solid" "purple" "24px sans-serif")
            (text canva "Mysterious Audio" 40 450 "solid" "purple" "15px sans-serif")
            (text canva "Normal Melody" 200 450 "solid" "purple" "15px sans-serif")
            (text canva "Mysterious Audio" 350 450 "solid" "purple" "15px sans-serif")
            (text canva "Second Listen" 355 480 "solid" "purple" "15px sans-serif")
            (rect 166 100)
            (rect 322 100))))