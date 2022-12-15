(import canvas)
(import html)
(import music)


(define canva (canvas 500 500))
canva
(rectangle canva 0 0 500 500 "solid" "white")
(import image)


(define clicks (vector 0))


;;; (define sqr) -> image?
;;; clr -> string? color?
;;; outputs a square of the given color 
(define sqr 
    (lambda (clr)
        (square 20 "solid" clr)))

;;; (define column) -> image?
;;; n -> integer? 
;;; i -> integer? positve? less than 25
;;; creates a column with a red 
(define column 
    (lambda (n i) 
        (cond 
            [(and (= i 24) (= i n))
                    (sqr "red")]
            [(= i 24) 
                (sqr "white")]
            [(= i n) 
                (above 
                    (column n (+ i 1))
                    (sqr "red"))]
            [else 
                (above 
                    (column n (+ i 1))
                    (sqr "white"))])))

(column 15 0)

;;; (define rectangles-helper) -> image?
;;; cl -> integer? 
;;; i -> integer? positive? less than 25
;;; constructs a grid of red squares based on how many clicks there are
(define rectangles-helper
    (lambda (cl i)
        (cond 
            [(>= cl i)
                    (beside (column i 0) (rectangles-helper cl (+ i 1)))]
            [(= i 24)
                (column -1 0)]
            [else 
                (beside (column -1 0) (rectangles-helper cl (+ i 1)))])))

;;; (define rectangles) -> image?
;;; constructs a grid of red squares based on how many clicks there are
(define rectangles
    (lambda ()
        (let 
            ([clicks (vector-ref clicks 0)])
            (rectangles-helper clicks 0))))


(rectangles-helper 23 0)

;;; (trigger-function) -> void?
;;; increments clicks vector every time it is called
(define trigger-function 
    (lambda () 
        (begin 
            (vector-set! clicks 0 
                (+ 1 (vector-ref clicks 0))))))

(trigger-function)
(trigger-function)
(trigger-function)
(vector-ref clicks 0)


;;; canvas-onclick -> null?
;;; every time canva is clicked execute trigger function 
(canvas-onclick canva
    (lambda ()
        (begin 
            (trigger-function))))

(animate-with
  (lambda (time)
    (begin
        (rectangle canva 0 0 500 500 "solid" "white")
        (draw-image canva (rectangles) 0 0))))

canva