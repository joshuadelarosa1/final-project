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

(define main-octave
    (lambda (midi-note n)
        (make-list n midi-note)))

(define higher-octave
    (lambda (midi-note n)
        (make-list n (- midi-note 12))))

(define lower-octave
    (lambda (midi-note n)
        (make-list n (+ midi-note 12))))

;;; (define raising-volume midi-note) -> list?
; (define lowering-volume
;    (lambda (midi-note n)
;     (map (lambda (x) (* x (/ 1 n))) (reverse (higher-octave midi-note n)))))

; (define raising-volume
;     (lambda (midi-note n)
;         (map (lambda (x) (* x (/ 1 n))) (lower-octave midi-note n))))

(define raising-pitch
    (list .01 .22 .03 .04 .05 .06 .07 .08 .09 .10
          .11 .12 .13 .14 .15 .16 .17 .18 .19 .20
          .21 .22 .23 .24 .25 .26 .27 .28 .29 .30
          .31 .32 .33 .34 .35 .36 .37 .38 .39 .40
          .41 .42 .43 .44 .45 .46 .47 .48 .49 .50
          .51 .52 .53 .54 .55 .56 .57 .58 .59 .60
          .61 .62 .63 .64 .65 .66 .67 .68 .69 .70
          .71 .72 .73 .74 .75 .76 .77 .78 .79 .80
          .81 .82 .83 .84 .85 .86 .87 .88 .89 .90
          .91 .92 .93 .94 .95 .96 .97 .98 .99 1))
    
;;; (shepard-tone note dur n) -> audio output
;;; note -> integer? (0 <= note <= 128)
;;; dur -> dur? 
;;; n -> integer? (0 < n)
;;; creates a shepard tone illusion starting at the octave specified by the user for a certain duration repeated n times
(define shepard-tone
    (lambda (midi-note dur n)
        (par (apply seq (map (lambda (x y) (volume x y)) (map (lambda (m) (note m dur)) (higher-octave midi-note n)) (reverse raising-pitch)))
             (apply seq (map (lambda (x y) (mod (bend y) x)) (map (lambda (m) (note m dur)) (main-octave midi-note n)) raising-pitch))
             (apply seq (map (lambda (x y) (volume x y)) (map (lambda (m) (note m dur)) (lower-octave midi-note n)) raising-pitch)))))

(shepard-tone 60 qn 100)

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


