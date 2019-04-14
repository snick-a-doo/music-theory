(require-extension srfi-78)

(load "/home/samv/programs/scheme/music/harmonic-analysis.scm")

;; Inversions
(check (analyze 'C '(C E G B)) => '(1 Mmaj7 0))
(check (analyze 'C '(E G B C)) => '(1 Mmaj7 1))
(check (analyze 'C '(G B C E)) => '(1 Mmaj7 2))
(check (analyze 'C '(B C E G)) => '(1 Mmaj7 3))

(check (analyze 'C '(C G B E C)) => '(1 Mmaj7 0))
(check (analyze 'C '(E E G C B)) => '(1 Mmaj7 1))
(check (analyze 'C '(G C E G B)) => '(1 Mmaj7 2))
(check (analyze 'C '(B G B C E)) => '(1 Mmaj7 3))

;; Quality
(check (analyze 'C '(C Eb G B)) => '(1 mmaj7 0))
(check (analyze 'C '(C Eb G Bb)) => '(1 m7 0))
(check (analyze 'C '(C Eb Gb Bb)) => '(1 o/7 0))
(check (analyze 'C '(C Eb Gb A)) => '(1 o7 0))

;; Degree
(check (analyze 'C '(D F A C)) => '(2 m7 0))
(check (analyze 'C '(E G B D)) => '(3 m7 0))
(check (analyze 'C '(F A C E)) => '(4 Mmaj7 0))
(check (analyze 'C '(G B D F)) => '(5 M7 0))
(check (analyze 'C '(A C E G)) => '(6 m7 0))
(check (analyze 'C '(B D F A)) => '(7 o/7 0))

;; Key
(check (analyze 'D '(C E G B)) => '(7 Mmaj7 0))
(check (analyze 'E '(C E G B)) => '(6 Mmaj7 0))
(check (analyze 'F '(C E G B)) => '(5 Mmaj7 0))
(check (analyze 'G '(C E G B)) => '(4 Mmaj7 0))
(check (analyze 'A '(C E G B)) => '(3 Mmaj7 0))
(check (analyze 'B '(C E G B)) => '(2 Mmaj7 0))

;; Triads
(check (analyze 'C '(C E G)) => '(1 M 0))
(check (analyze 'C '(D F A)) => '(2 m 0))
(check (analyze 'Eb '(Bb D G)) => '(3 m 1))
(check (analyze 'Eb '(Eb C Ab)) => '(4 M 2))
(check (analyze 'C '(F B D)) => '(7 o 2))

;; Example
(let ((anl (make-analyzer 'F#)))
  (check (anl '(F# A# F# C#)) => '(1 M 0))
  (check (anl '(G# B E# C#)) => '(5 M7 2))
  (check (anl '(A# A# F# C#)) => '(1 M 1))
  (check (anl '(B G# E# C#)) => '(5 M7 3)))

;; Format
(let ((anl (make-analyzer 'F#)))
  (check (format-chord (anl '(F# A# F# C#))) => "I")
  (check (format-chord (anl '(G# B E# C#))) => "V43")
  (check (format-chord (anl '(A# A# F# C#))) => "I6")
  (check (format-chord (anl '(B G# E# C#))) => "V42")
  (check (format-measure 'F# '((F# A# F# C#) (G# B E# C#) (A# A# F# C#) (B G# E# C#)))
         => "I V43 I6 V42")
  (check (format-analysis 'F# '(((F# A# F# C#) (G# B E# C#) (A# A# F# C#) (B G# E# C#))
                                ((A# F# F# C#) (B A# F# D#) (C# G# E# C#))))
         => "F#: I V43 I6 V42|I6 IV7 V")
  (check (format-analysis 'b '(((B B F# D))
                               ((C# A# F# E) (D B F# D) (E B G C#) (F# B F# D))
                               ((G A# E C#) (F# A# E C#) (B B D B))))
         => "b: i|V43 i6 iio/65 i64|viio42 V7 i")
  (check (format-analysis 'Bb '(((Bb Bb F D))
                                ((G Bb Eb Eb) (A G Eb C) (Bb F D Bb) (C F Eb A))
                                ((D F D Bb) (Eb G C Bb) (F F C A))))
         => "Bb: I|IV6 viio/7 I V43|I6 ii65 V"))
