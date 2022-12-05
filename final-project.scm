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
    (lambda (percent)
        (mod (dynamics (round (* percent 127))) note)))

(volume (note 60 qn) .4)

;;; (shepard-tone note dur n) -> audio output
;;; note -> integer? (0 <= note <= 128)
;;; dur -> dur? 
;;; n -> integer? (0 < n)
;;; creates a shepard tone illusion starting at the octave specified by the user for a certain duration repeated n times
; (define shepard-tone
;     (lambda note dur n)
;        (repeat n {??}))

;;; (define octave midi-note dur) -> list? 
;;; midi-note -> integer? (0 <= note <= 121) (since adding 7)
;;; dur -> dur? 
;;; returns list of notes in an octave 
(define octave 
    (lambda (midi-note dur)
        (list (note midi-note dur) (note (+ 1 midi-note dur)
            (note (+ 2 midi-note) dur) (note (+ 3 midi-note dur)
            (note (+ 4 midi-note dur) (note (+ 5 midi-note dur)
            (note (+ 6 midi-note dur) (note (+ 7 midi-note dur))))))))))

"Create 3 different actives put them par and then map t"





(define jukebox 
    (lambda (base-midi dur)
    {??}))


;;; (define jukebox-husk base-midi dur) -> music output
;;; base-midi -> integer? (0 <= note <= 121) (since adding 7)
;;; dur -> dur?
;;; calls jukebox with either the given arguments, or if those are wrong fills them in with a base 
;;; 60 midi value, and qn dur 
;;; eventually add in preference for whether want certain illusion or not 
(define jukebox-husk
    (lambda (base-midi dur)
        (cond 
            [(and (dur? dur) (and (< base-midi 121) (< 0 base-midi))) (jukebox base-midi dur)]
            [(dur? dur) (jukebox 60 dur)]
            [(and (< base-midi 121) (< 0 base-midi)) (jukebox base-midi qn)]
            [else (jukebox 60 qn)])))


