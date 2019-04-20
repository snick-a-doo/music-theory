;; Identify chord degree quality and inversion.
;; Copyright (C) 2019  Sam Varner  snick-a-doo@comcast.net
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Load extensions for Chicken.  Your Scheme may vary.
(require-extension srfi-1   ; list library
                   srfi-13) ; string library

;;; Utilities

;; Return lst with repeated elements removed.
(define (unique lst)
  (define (unique-tail lst out)
    (cond ((null? lst)
           (reverse out))
          ((or (null? out)
               (not (equal? (car lst) (car out))))
           (unique-tail (cdr lst) (cons (car lst) out)))
          (else
           (unique-tail (cdr lst) out))))
  (unique-tail lst '()))

;; Return the differences in adjacent elements of lst.
(define (pair-diff lst)
  (map (lambda (p1 p2)
         (cons (- (car p1) (car p2))
               (- (cdr p1) (cdr p2))))
       (cdr lst)
       lst))

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

;; Match target to the circularly-extended cdrs of the elements of alist with match-lists.  If
;; a match is found, return the car of alist and the index of the match.  Return #f for both if
;; a match is not found.
(define (look-up target alist)
  (define (extend lst)
    (append lst (take lst (- (length lst) 2))))
  (define (match? alist)
    (and (pair? alist)
         (match-lists target (extend (cdar alist)))))
  (define (look-up-inner alist)
    (let ((index (match? alist)))
      (cond ((null? alist)
             (values #f #f))
            (index
             (values (caar alist) index))
            (else
             (look-up-inner (cdr alist))))))
  (look-up-inner alist))

;;; Analysis Internals

;; Notes are encoded as the number of half steps from C for determining intervals and scale
;; degrees from C for determining inversions.
(define note-data '((C 0 0) (D 1 2) (E 2 4) (F 3 5) (G 4 7) (A 5 9) (B 6 11)))

(define sharp #\#)  ; Unicode sharp and flat signs can be used with the utf8 and
(define flat #\b)   ; utf8-srfi-13 extensions.

;; Return the natural note as a symbol and the number of half steps to add
;; according to the number of sharps or flats in the note name.
;; (parse-note Bb) => B -1
(define (parse-note note)
  (let ((note-str (symbol->string note)))
    (values (string->symbol
             (make-string 1 (string-ref note-str 0)))
            (- (string-count note-str sharp 1)
               (string-count note-str flat 1)))))

;; Given a note name as a symbol (e.g 'A, 'F#, 'Bb) return the degree of the
;; scale in the given key.
(define (note-degree note)
  (let-values (((natural modifier) (parse-note note)))
    (cadr (assq natural note-data))))

;; Given a note name as a symbol (e.g 'A, 'F#, 'Bb) return the number of half
;; steps from C to that note.
(define (note-half-step note)
  (let-values (((natural modifier) (parse-note note)))
    (+ (caddr (assq natural note-data))
       modifier)))

;; Constructor and accessors for encoded notes.
(define make-note-code cons)
(define note-code-interval car)
(define note-code-degree cdr)

;; Make a list of paired half-steps and degrees for the notes in 'chord'.
(define (encode-chord chord)
  ;;!! note-half-step and note-degree each parse the note.  Would be more efficient to parse
  ;;!! once.  Note that note-degree is used by chord-degree below.
  (let ((half-steps (map note-half-step chord))
        (degrees (map note-degree chord)))
    (map make-note-code half-steps degrees)))

;; Compare the notes of an encoded chord according to the interval.
(define (note-code< code-1 code-2)
  (< (note-code-interval code-1)
     (note-code-interval code-2)))

;; Shift encoded notes by octaves so the 1st note is the lowest.  Shifted notes may have
;; interval > 11 and degree > 6.
(define (raise-notes chord-code)
  (let ((first (car chord-code)))
    (cons first
          (map (lambda (code)
                 (let ((raise (note-code< code first)))
                   (cons (+ (note-code-interval code)
                            (if raise 12 0))
                         (+ (note-code-degree code)
                            (if raise 7 0)))))
               (cdr chord-code)))))

;; The pattern of intervals (half-steps) for triads.
(define triad-intervals '((o 3 3 6)
                          (m 3 4 5)
                          (M 4 3 5)))
(define triad-no-5th-intervals '((m 3 9)
                                 (M 4 8)))

;; The pattern of intervals (half-steps) for 7th chords.
(define 7th-intervals '((o7  3 3 3 3)
                        (m7  3 4 3 2)
                        (o/7 3 3 4 2)
                        (mM7 3 4 4 1)
                        (M7  4 3 3 2)
                        (MM7 4 3 4 1)))
(define 7th-no-5th-intervals '((o7 3 6 3)
                               (m7  3 7 2)
                               (mM7 3 8 1)
                               (M7  4 6 2)
                               (MM7 4 7 1)))

;; Scale degrees between notes for the kinds of chords we recognize.
(define chord-degrees '((triad-no-5th 2 5)
                        (triad        2 2 3)
                        (7th-no-5th   2 4 1)
                        (7th          2 2 2 1)))

;;!! The no-5th intervals and degrees can be found from the others by adding the 2nd and 3rd
;;!! elements.  The code could do it automatically, but I'm not sure it's worth the
;;!! complexity.  Would need to resolve the ambiguity between diminished and minor.

;; Return two values: a symbol that gives the chord quality (MM7 = major 7th,
;; m = minor triad, ...) and the inversion (0 = root, 1 = 1st, ...)
(define (quality notes)
  (let-values (((class inversion)
                (look-up (map cdr notes) chord-degrees)))
    (values (look-up (map car notes)
                     (case class
                       ((triad-no-5th) triad-no-5th-intervals)
                       ((triad) triad-intervals)
                       ((7th-no-5th) 7th-no-5th-intervals)
                       ((7th) 7th-intervals)
                       (else '())))
            inversion)))

;; Given a key name and the bass note name as symbols, and the inversion, return
;; the degree of the chord.
(define (chord-degree key chord inversion)
  ;; Upcase the key name for looking up the degree.
  (let ((key (string->symbol (string-titlecase (symbol->string key)))))
    (modulo (- (note-degree (car chord))
               (note-degree key)
               (* 2 inversion))
            7)))

;;; Analysis Interface

;; Return a list of the degree, quality, and inversion of 'chord' in 'key'.  'key' note name as
;; a symbol, 'chord' is a list of such symbols.  Return (#f #f #f) for if analysis is
;; unsuccessful. 
(define (analyze key chord)
  (let-values (((qual inv)
                (quality 
                 (pair-diff
                  (unique
                   (sort
                    (raise-notes
                     (encode-chord chord))
                    note-code<))))))
    (list (and inv (chord-degree key chord inv))
          qual
          inv)))

;; Return a function that gives the analysis of chords in 'key'.
(define (make-analyzer key)
  (lambda (chord)
    (analyze key chord)))

;;; Formatting

;; Inversion symbols fo triads and 7th chords.
(define inversions '#(("" . "7") ("6" . "65") ("64" . "43") (#f . "42")))

;; Roman numerals for scale degrees.
(define roman '#("I" "II" "III" "IV" "V" "VI" "VII"))

;; Return the string for the analyzed chord 'anl'.
(define (format-chord anl)
  ;; values is use as the identity function here and below.
  (if (every values anl)
      (let ((qual (symbol->string (cadr anl))))
        (let ((deg (vector-ref roman (car anl)))
              (maj (char=? (string-ref qual 0) #\M))
              (dim (char=? (string-ref qual 0) #\o))
              (7th (assq (cadr anl) 7th-intervals))
              (inv (vector-ref inversions (caddr anl))))
          (string-append ((if maj values string-downcase) deg)    ; Roman numeral
                         (if dim (string-trim-right qual #\7) "") ; possible diminished sign
                         ((if 7th cdr car) inv))))                ; inversion
      "--")) ; Analysis failed if there are any #f's in the analyzed chord.

;; Format a list of chords.
(define (format-measure key chords)
  (string-join (map format-chord
                    (map (lambda (notes)
                           (analyze key notes))
                         chords))
               " "))

;; Format a list of lists of chords.  Include bar lines between the strings for the sublists.
(define (format-analysis key measures)
  (string-append (symbol->string key)
                 ": "
                 (string-join (map (lambda (chords)
                                     (format-measure key chords))
                                   measures)
                              "|")))
