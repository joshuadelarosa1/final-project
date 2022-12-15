;;; final-project.scm
;;; Joshua Delarosa, Alma Ordaz, and Andy Chestovich
;;; 

(import music)
(import audio)

;;; (volume note percent) -> comp?
;;; note: comp?
;;; percent: float?
;;; Makes a MIDI note louder/quieter based on given percent.
(define volume
    (lambda (note percent)
        (mod (dynamics (round (* percent 127))) note)))

(volume (note 60 qn) .4)

;;; (main-octave midi-note n) -> list?
;;; midi-note: integer? (0 <= note <= 127)
;;; n: integer? n > 0
;;; Creates a list consisting of the main octave for Shepard Tone.
(define main-octave
    (lambda (midi-note n)
        (make-list n midi-note)))

;;; (higher-octave midi-note n) -> list?
;;; midi-note: integer? (0 <= note <= 127)
;;; n: integer? n > 0
;;; Creates a list consisting of the higher octave for Shepard Tone.
(define higher-octave
    (lambda (midi-note n)
        (make-list n (- midi-note 12))))

;;; (lower-octave midi-note n) -> list?
;;; midi-note: integer? (0 <= note <= 127)
;;; n: integer? n > 0
;;; Creates a list consisting of the lower octave for Shepard Tone.
(define lower-octave
    (lambda (midi-note n)
        (make-list n (+ midi-note 12))))

;;; (raising-volume n) -> list?
;;; n: integer? n > 0
;;; Creates a list of increasing numbers for the lower-octave volume in Shepard Tone.
(define raising-volume
    (lambda (n)
        (map (lambda (x) (* x (/ 1 n))) (range n))))

;;; (raising-pitch n) -> list?
;;; n: integer? n > 0
;;; Creates a list of increasing numbers for the main-octave pitch in Shepard Tone.
(define raising-pitch
    (lambda (n)
        (map (lambda (x) (* x (/ 1 n))) (range n))))

;;; (lowering-volume n) -> list?
;;; n: integer? n > 0
;;; Creates a list of decreasing numbers for the higher-octave volume in Shepard Tone.
(define lowering-volume
    (lambda (n)
        (reverse (map (lambda (x) (* x (/ 1 n))) (range n)))))
    
;;; (shepard-tone note dur n) -> audio output
;;; midi-note -> integer? (0 <= note <= 115)
;;; dur -> dur? 
;;; n -> integer? (n > 0)
;;; Create a Shepard Tone illusion starting at the note specified by the user for a certain duration n. 
(define shepard-tone
    (lambda (midi-note dur n)
        (let ([volumes (lambda (x y) (volume x y))]
              [make-note (lambda (m) (note m dur))]
              [pitch-modifier (lambda (x y) (mod (bend y) x))])
            (par (apply seq (map volumes (map make-note (higher-octave midi-note n)) (lowering-volume n)))
                 (apply seq (map pitch-modifier (map make-note (main-octave midi-note n)) (raising-pitch n)))
                 (apply seq (map volumes (map make-note (lower-octave midi-note n)) (raising-volume n)))))))

(shepard-tone 60 qn 20)
(shepard-tone 60 en 80)
(shepard-tone 80 hn 10)
(shepard-tone 20 en 30)

;;; (define risset-rhythm __ __) -> composition?
;;;

(define risset-rhythm {??})

;;; (define octave midi-note dwn/up) -> integer? 
;;; midi-note -> integer? (0 <= note <= 121) (since adding 7)
;;; dwn/up -> integer? [-5 5]
;;; This returns a middi-note however many octaves up or down the user indicates thorugh dwn/up. 
(define octave 
    (lambda (midi-note dwn/up)
        (+ midi-note (* dwn/up 12))))

(octave 60 -1)

;;; (twinkle-twinkle-litle-star) --> list?
;;; list of the midi-note values that make up twinkle-twinkle-little-star

(define twinkle-twinkle-litle-star
    (list 60 60 67 67 69 69 67
          65 65 64 64 62 62 60
          67 67 65 65 64 64 62
          67 67 65 65 64 64 62))

;;; (happy-birthday) --> list?
;;; list of the midi-note values that make up happy-birthday

(define happy-birthday
    (list 60 60 62 60 65 64
          60 60 62 60 67 65
          60 60 60 69 67 64 62
          70 70 69 65 67 65))

;;; (fur-elise) --> list?
;;; list of the midi-note values that make up fur-elise

(define fur-elise
    (list 64 63 64 63 64 71 62 60 69
          60 64 69 71 64 69 71 60
          64 63 64 63 64 71 62 60 69
          60 64 69 71 64 69 71 60
          71 60 62 64 67 65 64 62 64 64 62 60 64 62 60 64
          ))

;;; (jingle-bells) --> list?
;;; list of the midi-note values that make up jingle-bells

(define jingle-bells
    (list  64 64 64 64 67 60 62 64 
           65 65 65 64 64 
           64 64 64 64 67 60 62 64
           65 65 65 64 64
           64 64 67 67 65 62 60))

;;; (original melody) --> composition?
;;;     melody : list?
;;; Plays the origional given melody before going through the illusion.

(define original
    (lambda (melody)
    (apply seq (map (lambda (x) (note x qn)) melody))))
(original twinkle-twinkle-litle-star)
(original happy-birthday)
(original fur-elise)
(original jingle-bells)

;;; (mysterious-melodies melody) --> compositon?
;;;     melody : string?
;;; This will create a mysterious melody illusion given the chosen melody 

(define mysterious-melodies
    (lambda (melody)
        (let ([illussion-notes (map (lambda (x) (octave x (- (random 3) 1))) melody)])
        (apply seq (map (lambda (x) (note x qn)) illussion-notes))
        )))

(mysterious-melodies twinkle-twinkle-litle-star)
(mysterious-melodies happy-birthday)


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


