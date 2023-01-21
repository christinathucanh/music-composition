; Project: This project is a decomposition of the song "I'm Good"
; by Bebe Rexha and David Guetta.
; Authors: Anh Thuc (Christina) Vu, John Miller, Luke Walters, Luke Caruso-Thompson
; Date: 2022-10-12
(import music) 
(import canvas)
(import html)

; Will play all of the midi notes from 40 to 107 on the given dur all at once
(define slam
    (lambda (dur)
        (apply par
            (map (lambda (x) (note x dur))
                 (range 40 107)))))

;Will play a sequence of all the midi note values starting at 40 going to 77 with decreased dynamics
(define slide-right
    (lambda (dur)
        (mod (dynamics 80)
          (apply seq
            (map (lambda (x) (note x dur))
                 (range 40 77))))))

; Will play a sequence of all the midi note values starting at 77 going to 40 with decreased dynamics
(define slide-left
    (lambda (dur)
      (mod (dynamics 80)
        (apply seq
            (map (lambda (x) (note x dur))
                 (reverse (range 40 77)))))))

; C note of the fourth octave
(define C4
  (lambda (dur)
    (note 60 dur)))

; C sharp note of the fourth octave
(define C#4
  (lambda (dur)
    (note 61 dur)))

; D note of the fourth octave
(define D4
  (lambda (dur)
    (note 62 dur)))

; D sharp note of the fourth octave
(define D#4
  (lambda (dur)
    (note 63 dur)))

; E note of the fourth octave
(define E4
  (lambda (dur)
    (note 64 dur)))
 
; F note of the fourth octave
(define F4
  (lambda (dur)
    (note 65 dur)))

; F sharp note of the fourth octave
(define F#4
  (lambda (dur)
    (note 66 dur)))

; G note of the fourth octave
(define G4
  (lambda (dur)
    (note 67 dur)))

; G sharp note of the fourth octave
(define G#4
  (lambda (dur)
    (note 68 dur)))

; A note of the fourth octave
(define A4
  (lambda (dur)
    (note 69 dur)))

; A sharp note of the fourth octave
(define A#4
  (lambda (dur)
    (note 70 dur)))

; B flat note of the fourth octave
(define B-flat
  (lambda (dur)
    (note 71 dur)))

; B note of the fourth octave
(define b
  (lambda (dur)
    (note 72 dur)))

; E flat note of the fifth octave
(define e-flat
  (lambda (dur)
    (note 76 dur)))

; D note of the fifth octave
(define d
  (lambda (dur)
    (note 75 dur)))

; C note of the fifth octave
(define C
  (lambda (dur)
    (note 73 dur)))

; c-minor chord of the third octave
(define c-minor-chord
  (lambda (dur)
    (par (note 55 dur)(note 51 dur)(note 48 dur))))

; G-minor-chord of the second octave
(define G-minor-chord
  (lambda (dur)
    (par (note 50 dur)(note 46 dur)(note 43 dur))))

; f major chord of the second octave
(define f-major-chord
  (lambda (dur)
    (par (note 48 dur)(note 45 dur)(note 41 dur))))

; e flay major chord of the second octave
(define e-flat-major-chord
  (lambda (dur)
    (par (note 46 dur)(note 43 dur)(note 39 dur))))

; The songs base composition there is dynamics on the G4 quarter note near the end
(define im-good-main
  (seq (B-flat qn) (D#4 qn)(G4 qn)(B-flat qn)
    (C qn)(F4 qn)(A4 qn)(B-flat hn)(G4 qn)
    (B-flat qn)(d qn)(e-flat qn) (mod (dynamics 30) (G4 qn)) (d qn) (C qn)))

; The composition of the four different chords all put together in a sequence and have lowered dynamics
(define chords
  (mod (dynamics 50) (seq (G-minor-chord wn)(f-major-chord wn)(e-flat-major-chord wn)(c-minor-chord wn))))

; (random-velocity max min) -> integer?
;     max: integer? (min <= max <= 127)
;     min: integer? (0 < min <= max)
;;; Generates a random number between min and max (inclusive).
(define random-velocity
    (lambda (max min)
        (let ([maybe-result (random (+ max 1))])
            (cond
                [(< maybe-result min) min]
                [else maybe-result]))))

;;; (randomize-dynamics prev) -> integer?
;;;     prev: integer? (0 <= prev <= 127)
;;; Generates a random number based on prev.
;;; There is a 50% chance the number will be the same as prev,
;;; a 25% chance the number will be less than prev, and a
;;; 25% it will be louder than prev.
;;; The number will never be less than 1 or greater than 127.
(define randomize-dynamics
    (lambda (prev)
        (let ([chance (random 100)])
            (cond
                [(< chance 50) prev]
                [(< chance 75) (random-velocity prev 10)]
                [else (random-velocity 127 prev)]))))

;;; (generate-random-dynamics num) -> list? of integer?
;;;     num: integer? (num >= 0)
;;; Generates a list of num random integers between 1 and 127.
(define generate-random-dynamics
    (lambda (num)
        (fold-right
            (lambda (x acc) (cons (randomize-dynamics (car acc)) acc))
            (list (random 128))
            (range (- num 1)))))

;;; (apply-random-dynamics notes velocities) -> list? of composition?
;;;     notes: list? of composition?
;;;     velocities: list? of integer? (0 < i <= 127)
;;; Applies the given dynamics to the compositions. Lists must be of the same size.
(define apply-random-dynamics
    (lambda (notes velocities)
        (map
            (lambda (n vel) (mod (dynamics vel) n))
            notes
            velocities)))

 

;;; (makes-random-left num midi-val dur)
;;;     num : integer? that is within the the range 0 50
;;;     midi-val : integer? must be an acceptable midi note value
;;;     dur : duration?
;;; Takes a number, midi-value, and duration and randomly moves the midi-value down by a number
;;; within the range 0 to num. Then it creates a note with the randomized midi value and given duration
(define makes-random-left
  (lambda (num midi-val dur)
    (note (- midi-val (random num)) dur)))

;;; (makes-random-left num midi-val dur)
;;;     num : integer?  that is within the the range 0 30
;;;     midi-val : integer? must be an acceptable midi note value
;;;     dur : duration?
;;; Takes a number, midi-value, and duration and randomly moves the midi-value up by a number
;;; within the range 0 to num. Then it creates a note with the randomized midi value and given duration
(define makes-random-right
  (lambda (num midi-val dur)
    (note (- midi-val (random num)) dur)))

;;; Creates a composition of random notes inteded for base-comp-with-random-left
(define makes-random-for-im-good-left
  (lambda (num)
    (let*
      ([rand (lambda (x) (makes-random-left num x qn))])
      (mod (dynamics 40)
        (seq (apply seq (map rand (list 71 63 67 71 73 65 69)))
        (makes-random-left num 71 hn)
        (apply seq (map rand (list 67 71 75 76)))
        (mod (dynamics 30) (rand 67))
        (apply seq (map rand(list 75 73))))))))

;;; Creates a composition of random notes inteded for base-comp-with-random-right
(define makes-random-for-im-good-right
  (lambda (num)
    (let*
      ([rand (lambda (x) (makes-random-right num x qn))]
       [song-tempo (tempo qn 5000)])
        (seq (apply seq  (apply-random-dynamics (map rand (list 71 63 67 71 73 65 69)) (generate-random-dynamics 7)))
        (par (slam qn) (makes-random-right num 71 hn))
        (par (mod song-tempo (slide-right qn)) (apply seq (map rand (list 67 71 75 76))))
        (par (mod song-tempo (slide-left qn)) (mod (dynamics 30) (makes-random-right num 67 qn)))
        (apply seq (map rand(list 75 73)))))))
 
;;; base-comp but par with makes-random-for-im-good-left
(define base-comp-with-random-left
  (repeat 2 (mod (tempo qn 260)
    (par
      (makes-random-for-im-good-left 4)
      im-good-main
      chords))))

;;; base-comp but par with makes-random-for-im-good-right
(define base-comp-with-random-right
  (repeat 2 (mod (tempo qn 260)
    (par
      (makes-random-for-im-good-right 11)
      im-good-main
      chords))))

;;; m: MIDI note?
;;; n: number?
;;; dur: duration?
;;; Returns the collection of notes that evenly spaced notes that fit the duration of the original note.
(define roll
  (lambda (m n d)
    (mod percussion (note m (dur (numerator d)(* (denominator d)(/ 2)))))))

;;; base-comp without any elements of randomness and a drum line added underneath
(define base-comp
  (repeat 2 (mod (tempo qn 250)
    (par
      (seq (repeat 7 (roll 35 1 qn))
          (rest qn))
          im-good-main
          chords))))

;;; basic canvas used as base
(define canv (canvas 500 500))

;;;this is the base canvas for which the canvas-onclick procedure can manipulate and change based on the input of the user's
;;; button click.
(begin
  (rectangle canv 0 0 500 500 "solid" "white")
  (text canv "answer this:"  0 30 "solid" "blue" "24px sans-serif")
  (text canv "what is 75x225" 10 50 "solid" "blue" "24px sans-serif")
  (text canv "a) 16875" 10 70 "solid" "blue" "20px sans-serif")
  (text canv "b) 16822" 10 90 "solid" "blue" "20px sans-serif")
  (text canv "c) 16879" 10 110 "solid" "blue" "20px sans-serif")
  (text canv "next question" 250 50 "solid" "black" "24px sans-serif")
  (text canv "next question" 250 330 "solid" "black" "24px sans-serif")
  (text canv "next question" 10 330 "solid" "black" "24px sans-serif"))

canv

;;;Canvas-onclick takes the users's click coordinates as input, and divides up the canvas into four separate parts. Each part pertains to a separate
;;; math question, which is shown in the top left quadrand. If the top left corner is clicked, an error message will appear.
(canvas-onclick canv
  (lambda (x y)
    (let ([x-gt-quad (> x 250)]
          [y-gt-quad (> y 250)])
      (if (and x-gt-quad (< y 250))
        (begin
          (rectangle canv 0 0 500 500 "solid" "white")
          (text canv "answer this:"  0 30 "solid" "blue" "24px sans-serif")
          (text canv "what is 3375x5" 10 50 "solid" "blue" "24px sans-serif")
          (text canv "a) 16875" 10 70 "solid" "blue" "20px sans-serif")
          (text canv "b) 16075" 10 90 "solid" "blue" "20px sans-serif")
          (text canv "c) 18879" 10 110 "solid" "blue" "20px sans-serif")
          (text canv "next question" 250 50 "solid" "black" "24px sans-serif")
          (text canv "next question" 250 330 "solid" "black" "24px sans-serif")
          (text canv "next question" 10 330 "solid" "black" "24px sans-serif"))
       (if (and x-gt-quad y-gt-quad)
        (begin
          (rectangle canv 0 0 500 500 "solid" "green")
          (text canv "answer this:"  0 30 "solid" "blue" "24px sans-serif")
          (text canv "what is 2x 8437.5" 10 50 "solid" "blue" "24px sans-serif")
          (text canv "a) 16875" 10 70 "solid" "blue" "20px sans-serif")
          (text canv "b) 16002" 10 90 "solid" "blue" "20px sans-serif")
          (text canv "c) 18875" 10 110 "solid" "blue" "20px sans-serif")
          (text canv "next question" 250 50 "solid" "black" "24px sans-serif")
          (text canv "next question" 250 330 "solid" "black" "24px sans-serif")
          (text canv "next question" 10 330 "solid" "black" "24px sans-serif"))
        (if (and (< x 250) y-gt-quad)
          (begin
            (rectangle canv 0 0 500 500 "solid" "yellow")
            (text canv "answer this:"  0 30 "solid" "blue" "24px sans-serif")
            (text canv "what is 4x 4218.75" 10 50 "solid" "blue" "24px sans-serif")
            (text canv "a) 16875" 10 70 "solid" "blue" "20px sans-serif")
            (text canv "b) 16870" 10 90 "solid" "blue" "20px sans-serif")
            (text canv "c) 10879" 10 110 "solid" "blue" "20px sans-serif")
            (text canv "next question" 250 50 "solid" "black" "24px sans-serif")
            (text canv "next question" 250 330 "solid" "black" "24px sans-serif")
            (text canv "next question" 10 330 "solid" "black" "24px sans-serif"))
          (begin
            (rectangle canv 0 0 500 500 "solid" "orange")
            (text canv "erroneous button click"  0 30 "solid" "blue" "24px sans-serif")
            (text canv "next question" 250 50 "solid" "black" "24px sans-serif")
            (text canv "next question" 250 330 "solid" "black" "24px sans-serif")
            (text canv "next question" 10 330 "solid" "black" "24px sans-serif"))))))))

(define trigger-function-1
  (lambda () (play-composition base-comp)))

(define trigger-function-2
  (lambda () (play-composition base-comp-with-random-left)))

(define trigger-function-3
  (lambda () (play-composition base-comp-with-random-right)))

(define button-one
  (button "correct option" "answer a)"))

button-one

(define button-two
  (button "incorrect option" "answer b)"))

button-two

(define button-three
  (button "more incorrect option" "answer c)"))

button-three

(button-onclick button-one trigger-function-1)

(button-onclick button-two trigger-function-2)

(button-onclick button-three trigger-function-3)
