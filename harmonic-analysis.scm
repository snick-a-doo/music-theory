(require-extension srfi-1
                   utf8
                   utf8-srfi-13)

;;; Utilities

;; Return lst with repeated (eq?) elements removed.
(define (unique lst)
  (define (unique-tail lst out)
    (cond ((null? lst)
           (reverse out))
          ((or (null? out)
               (not (eq? (car lst) (car out))))
           (unique-tail (cdr lst) (cons (car lst) out)))
          (else
           (unique-tail (cdr lst) out))))
  (unique-tail lst '()))

;; Return the differences in adjacent elements of lst.
(define (diff lst)
  (map - (cdr lst) lst))

;; If sub is a sublist of lst, return the index of the 1st occurrence of sub,
;; else return #f.
(define (match-lists sub lst)
  (let ((max-n (- (length lst) (length sub))))
    (define (find lst n)
      (cond ((> n max-n)
             #f)
            ((every eq? sub lst)
             n)
            (else
             (find (cdr lst) (+ n 1)))))
    (find lst 0)))

;;; Analysis

;; Degrees and half-steps from C. Notes are encoded as the number of half steps
;; from C for determining intervals.  Degree are for the key of C.  The degree
;; in other keys is found by subtracting the degree of the key.
(define note-data '((C 1 0) (D 2 2) (E 3 4) (F 4 5) (G 5 7) (A 6 9) (B 7 11)))

(define sharp #\#)  ; Unicode sharp and flat signs can be used with the utf8 and
(define flat #\b)   ; utf8-srfi-13 extensions.

;; Return the natural note as a symbol and the number of half steps to add
;; according to the number of sharps or flats in the note name.
;; (decode-note Bb) => B -1
(define (decode-note note)
  (let ((note-str (symbol->string note)))
    (values (string->symbol
             (make-string 1 (string-ref note-str 0)))
            (- (string-count note-str sharp 1)
               (string-count note-str flat 1)))))

;; Given a note name as a symbol (e.g 'A, 'F#, 'Bb) return the degree of the
;; scale in the given key.
(define (note->degree note)
  (let-values (((natural modifier) (decode-note note)))
    (cadr (assq natural note-data))))

;; Given a note name as a symbol (e.g 'A, 'F#, 'Bb) return the number of half
;; steps from C to that note.
(define (note->half-step note)
  (let-values (((natural modifier) (decode-note note)))
    (+ (caddr (assq natural note-data))
       modifier)))

;; The pattern of intervals for the chords we recognize.  Intervals are give as
;; a number of half steps.  Triads must come first to avoid false matches with
;; 7th chords.
(define chord-intervals '((o     3 3 6)
                          (m     3 4 5)
                          (M     4 3 5)
                          (o7    3 3 3 3)
                          (o/7   3 3 4 2)
                          (m7    3 4 3 2)
                          (mmaj7 3 4 4 1)
                          (M7    4 3 3 2)
                          (Mmaj7 4 3 4 1)))

;; Return two values: a symbol that gives the chord quality (Mmaj7 = major 7th,
;; m = minor triad, ...) and the inversion (0 = root, 1 = 1st, ...)
(define (quality intervals)
  (define (inversion 7th)
    (match-lists intervals
                 (append 7th (take 7th 2))))
  (define (find-quality chord-intervals)
    (if (null? chord-intervals)
        (error "Couldn't identify intervals." intervals)
        (let ((inv (inversion (cdar chord-intervals))))
          (if inv
              (values (caar chord-intervals) inv)
              (find-quality (cdr chord-intervals))))))
  (find-quality chord-intervals))

;; For elements in the cdr of 'half-steps', raise by an octave (add 12) if it's <
;; the car.
(define (raise half-steps)
  (let ((first (car half-steps)))
    (cons first
          (map (lambda (hs)
                 (+ hs (if (< hs first) 12 0)))
               (cdr half-steps)))))

;; Given a key name and the bass note name as symbols, and the inversion, return
;; the degree of the chord.
(define (key-degree key bass-note inversion)
  ;; Upcase the key name for looking up the degree.
  (let ((key (string->symbol (string-titlecase (symbol->string key)))))
    (+ (modulo (- (note->degree bass-note)
                  (note->degree key)
                  (* 2 inversion))
               7)
       1)))

;; Return a list of the degree, quality, and inversion of 'chord' in 'key'.
(define (analyze key chord)
  (let-values (((qual inv)
                (quality
                 (diff
                  (unique
                   (sort
                    (raise
                     (map note->half-step chord))
                    <))))))
    (list (key-degree key (car chord) inv)
          qual
          inv)))

;; Return a function that gives the analysis of chords in 'key'.
(define (make-analyzer key)
  (lambda (chord)
    (analyze key chord)))

;;; Formatting

;; Inversions of triads and 7th chords.
(define inversions '#(("" . "7") ("6" . "65") ("64" . "43") (#f . "42")))

(define roman '#(("I" . "i") ("II" . "ii") ("III" . "iii") ("IV" . "iv")
                 ("V" . "v") ("VI" . "vi") ("VII" . "vii")))

(define (format-chord anl)
  (let ((deg (vector-ref roman (- (car anl) 1)))
        (maj (or (eq? (cadr anl) 'M)
                 (eq? (cadr anl) 'M7)
                 (eq? (cadr anl) 'Mmaj7)))
        (7th (or (eq? (cadr anl) 'o)
                 (eq? (cadr anl) 'o/7)
                 (eq? (cadr anl) 'm7)
                 (eq? (cadr anl) 'mmaj7)
                 (eq? (cadr anl) 'M7)
                 (eq? (cadr anl) 'Mmaj7)))
        (inv (vector-ref inversions (caddr anl))))
    (string-append ((if maj car cdr) deg)
                   (if (or (eq? (cadr anl) 'o)
                           (eq? (cadr anl) 'o/))
                       (symbol->string (cadr anl))
                       "")
                   ((if 7th cdr car) inv))))

(define (format-measure key chords)
  (string-join (map format-chord
                    (map (lambda (notes)
                           (analyze key notes))
                         chords))
               " "))

(define (format-analysis key measures)
  (string-append (symbol->string key)
                 ": "
                 (string-join (map (lambda (chords)
                                     (format-measure key chords))
                                   measures)
                              "|")))
