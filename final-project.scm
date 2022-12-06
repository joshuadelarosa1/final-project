;;; final-project.scm
;;; Joshua Delarosa, Alma Ordaz, and Andy Chestovich
;;; 

(import music)
(import audio)

;;; (volume note percent) -> comp?
;;; note: comp?
;;; percent: float?
;;; Makes a MIDI note louder based on given percent.
(define volume
    (lambda (note percent)
        (mod (dynamics (round (* percent 127))) note)))

(volume (note 60 qn) .4)

;;; (define octave midi-note dwn/up) -> integer? 
;;; midi-note -> integer? (0 <= note <= 121) (since adding 7)
;;; dwn/up -> integer? [-5 5]
;;; This returns a middi-note however many octaves up or down the user indicates thorugh dwn/up. 

(define octave 
    (lambda (midi-note dwn/up)
        (+ midi-note (* dwn/up 12))))

(octave 60 -1)

;;; (define raising-volume midi-note) -> list?
(define raising-volume
    (list .3 .4 .5 .6 .7 .8 .9 1))

(define lowering-volume
    (list 1 .9 .8 .7 .6 .5 .4 .3))
    
;;; (shepard-tone note dur n) -> audio output
;;; note -> integer? (0 <= note <= 128)
;;; dur -> dur? 
;;; n -> integer? (0 < n)
;;; creates a shepard tone illusion starting at the octave specified by the user for a certain duration repeated n times
(define shepard-tone
    (lambda (midi-note dur n)
       (repeat n
        (par (apply seq (map (lambda (x y) (volume x y)) (octave (- midi-note 8)) raising-volume))
             (apply seq (map (lambda (m) (note m dur)) (octave midi-note)))
             (apply seq (map (lambda (x y) (volume x y)) (octave (+ midi-note 8)) lowering-volume))))))

(shepard-tone 60 qn 1)

;;; (define risset-rhythm __ __) -> composition?
;;;

(define risset-rhythm {??})


;;; (twinkle-twinkle-litle-star) --> list?
;;; list of the midi-note values that make up twinkle-twinkle-little-star

(define twinkle-twinkle-litle-star
    (list 60 60 67 67 69 69 67
          65 65 64 64 62 62 60
          67 67 65 65 64 64 62
          67 67 65 65 64 64 62))

;;; (original melody) --> composition?
;;;     melody : list?
;;; Plays the origional given melody before going through the illusion.

(define original
    (lambda (melody)
    (apply seq (map (lambda (x) (note x qn)) melody))))
(original twinkle-twinkle-litle-star)

;;; (mysterious-melodies melody) --> compositon?
;;;     melody : string?
;;; This will create a mysterious melody illusion given the chosen melody 

(define mysterious-melodies
    (lambda (melody)
        (let ([illussion-notes (map (lambda (x) (octave x (- (random 3) 1))) melody)])
        (apply seq (map (lambda (x) (note x qn)) illussion-notes))
        )))

(mysterious-melodies twinkle-twinkle-litle-star)

;;; (define jukebox base-midi dur choice n_ -> music output?
;;;base-midi -> integer? (10 <= note <= 112) (since adding going octaves above and below)
;;; dur -> dur?
;;; choice -> integer? (0 <= choice <= 3) 
;;; n -> integer? positive? 
;;; melody -> integer? 0 or 1
(define jukebox 
    (lambda (base-midi dur choice n melody)
        (cond 
            [(= 0 choice) (jukebox base-midi dur (+ 1 (random 3)))]
            [(= 1 choice) (shepard-tone base-midi dur n)]
            [(= 2 choice) (mysterious-melodies melody)]
            [(= 3 choice) (risset-rhythm {??})])))


;;; (define jukebox-husk base-midi dur) -> music output
;;; base-midi -> integer? (10 <= note <= 112) (since adding going octaves above and below)
;;; dur -> dur?
;;; illusion-choice -> string? containing 
;;; n -> integer? positive? 
;;; melody -> integer? 0 or 1
;;; calls jukebox with either the given arguments, or if those are wrong fills them in with a base 
;;; 60 midi value, and qn dur, and repeats n times
(define jukebox-husk
    (lambda (base-midi dur illusion-choice n melody)
        (let 
            ([choice 
                (cond 
                    [(equal? illusion-choice "shepard tone") 1]
                    [(equal? illusion-choice "mysterious melody") 2]
                    [(equal? illusion-choice "rissett rhythm") 3]
                    [else 0])])
            (cond 
                [(and (dur? dur) (and (< base-midi 112) (< 0 base-midi))) (jukebox base-midi dur choice n melody)]
                [(dur? dur) (jukebox 60 dur choice n melody)]
                [(and (< base-midi 112) (< 0 base-midi)) (jukebox base-midi qn choice n melody)]
                [else (jukebox 60 qn choice n melody)]))))


