(require-extension srfi-78)

;; Inversions
(check (analyze 'C '(C E G B)) => '(0 MM7 0))
(check (analyze 'C '(E G B C)) => '(0 MM7 1))
(check (analyze 'C '(G B C E)) => '(0 MM7 2))
(check (analyze 'C '(B C E G)) => '(0 MM7 3))

(check (analyze 'C '(C G B E C)) => '(0 MM7 0))
(check (analyze 'C '(E E G C B)) => '(0 MM7 1))
(check (analyze 'C '(G C E G B)) => '(0 MM7 2))
(check (analyze 'C '(B G B C E)) => '(0 MM7 3))

;; Quality
(check (analyze 'C '(C Eb G B)) => '(0 mM7 0))
(check (analyze 'C '(C Eb G Bb)) => '(0 m7 0))
(check (analyze 'C '(C Eb Gb Bb)) => '(0 o/7 0))
(check (analyze 'C '(C# E G Bb)) => '(0 o7 0))

;; Degree
(check (analyze 'C '(D F A C)) => '(1 m7 0))
(check (analyze 'C '(E G B D)) => '(2 m7 0))
(check (analyze 'C '(F A C E)) => '(3 MM7 0))
(check (analyze 'C '(G B D F)) => '(4 M7 0))
(check (analyze 'C '(A C E G)) => '(5 m7 0))
(check (analyze 'C '(B D F A)) => '(6 o/7 0))

;; Key
(check (analyze 'D '(C E G B)) => '(6 MM7 0))
(check (analyze 'E '(C E G B)) => '(5 MM7 0))
(check (analyze 'F '(C E G B)) => '(4 MM7 0))
(check (analyze 'G '(C E G B)) => '(3 MM7 0))
(check (analyze 'A '(C E G B)) => '(2 MM7 0))
(check (analyze 'B '(C E G B)) => '(1 MM7 0))

;; Triads
(check (analyze 'C '(C E G)) => '(0 M 0))
(check (analyze 'C '(D F A)) => '(1 m 0))
(check (analyze 'Eb '(Bb D G)) => '(2 m 1))
(check (analyze 'Eb '(Eb C Ab)) => '(3 M 2))
(check (analyze 'C '(F B D)) => '(6 o 2))

;; Example
(let ((anl (make-analyzer 'F#)))
  (check (anl '(F# A# F# C#)) => '(0 M 0))
  (check (anl '(G# B E# C#)) => '(4 M7 2))
  (check (anl '(A# A# F# C#)) => '(0 M 1))
  (check (anl '(B G# E# C#)) => '(4 M7 3)))

;; Format
(let ((anl (make-analyzer 'F#)))
  (check (format-chord (anl '(F# A# F# C#))) => "I")
  (check (format-chord (anl '(G# B E# C#))) => "V43")
  (check (format-chord (anl '(A# A# F# C#))) => "I6")
  (check (format-chord (anl '(B G# E# C#))) => "V42")
  (check (format-measure 'F# '((F# A# F# C#) (G# B E# C#) (A# A# F# C#) (B G# E# C#)))
         => "I V43 I6 V42")
  (check (format-analysis 'C '(((C G E C) (C A F C) (C G E C))
                               ((A F F C) (F A F C) (F A F D))
                               ((G G E C) (G G D B))
                               ((C G E C) (D G D B) (E G C C))
                               ((F A F C) (D A F D)) ((G G E C) (G G D B)) ((C G E C))))
         => "C: I IV64 I|IV6 IV ii6|I64 V|I V64 I6|IV ii|I64 V|I")
  
  (check (format-analysis 'F# '(((F# A# F# C#) (G# B E# C#) (A# A# F# C#) (B G# E# C#))
                                ((A# F# F# C#) (B A# F# D#) (C# G# E# C#))
                                ((F# A# F# C#) (E# B G# C#) (D# B F# B) (D# B E# G#))
                                ((C# C# F# A#) (C# B E# G#) (F# A# C# F#))))
         => "F#: I V43 I6 V42|I6 IV7 V|I V65 IV6 viio/42|I64 V7 I")
  (check (format-analysis 'b '(((B B F# D))
                               ((C# A# F# E) (D B F# D) (E B G C#) (F# B F# D))
                               ((G A# E C#) (F# A# E C#) (B B D B))
                               ((E G C# A#) (D F# D B) (C# F# E A#) (B F# D B))
                               ((E G E B) (F# F# E A#) (B F# D B))))
         => "b: i|V43 i6 iio/65 i64|viio42 V7 i|viio43 i6 V43 i|iv V7 i")
  ;; Mistakenly shown as V64 on answer key----------------^
  (check (format-analysis 'Bb '(((Bb Bb F D))
                                ((G Bb Eb Eb) (A G Eb C) (Bb F D Bb) (C F Eb A))
                                ((D F D Bb) (Eb G C Bb) (F F C A))
                                ((Eb A F C) (D Bb F Bb) (G Bb Eb Bb) (G C Eb A))
                                ((F D F Bb) (F C Eb A) (Bb Bb D Bb))))
         => "Bb: I|IV6 viio/7 I V43|I6 ii65 V|V42 I6 IV6 viio/42|I64 V7 I")
  (check (format-analysis 'a '(((A C E A))
                               ((B D F G#) (C C E A) (D B F G#) (E C E A))
                               ((F B D G#) (E C E A) (E B E G#) (F A D A))
                               ((F B D G#) (E B D G#) (F A C A) (D B F A))
                               ((E C E A) (E B D G#) (A A C A))))
         => "a: i|viio65 i6 viio43 i64|viio42 i64 V iv6|viio42 V7 VI iio/65|i64 V7 i"))
