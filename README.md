# Music Theory

## Harmonic Analysis
`harmonic-analysis.scm` defines Scheme functions for determining the degree, quality, and inversion of chords.

The `analyze` function takes a key and a list of notes and returns a list giving a zero-based number for the degree of the chord, a symbol for the chord quality, and a number for the inversion.
```
  (analyze 'C '(C E G)) => (0 M 0)
  (analyze 'Bb '(Bb Eb G D)) => (3 MM7 2)
```

`make-analyzer` returns a function for analyzing in the given key.
```
  (define anl (make-analyzer 'F#))
  (anl '(F# A# F# C#)) => '(0 M 0)
  (anl '(G# B E# C#)) => '(4 M7 2)
```

Roman-numeral notation is produced with the "format" functions.
```
  (format-chord (anl '(F# A# F# C#))) => "I"
  (format-chord (anl '(G# B E# C#))) => "V43"

  (format-measure 'F# '((F# A# F# C#) (G# B E# C#) (A# A# F# C#) (B G# E# C#))) => "I V43 I6 V42"

  (format-analysis 'b '(((B B F# D))
                        ((C# A# F# E) (D B F# D) (E B G C#) (F# B F# D))
                        ((G A# E C#) (F# A# E C#) (B B D B))
                        ((E G C# A#) (D F# D B) (C# F# E A#) (B F# D B))
                        ((E G E B) (F# F# E A#) (B F# D B))))
         => "b: i|V43 i6 iio/65 i64|viio42 V7 i|viio43 i6 V43 i|iv V7 i")
```

`harmonic-analysis.scm` uses SRFI-1 (list library) and SRFI-13 (string library).  The unit tests in `test-harmonic-analysis.scm` use SRFI-78 (lightweight testing).  The code was developed with Chicken Scheme, but should run with any R5RS Scheme after replacing the `require-extension` calls with your Scheme's equivalent.

This program is free software, and you are welcome to redistribute it under certain conditions; see `LICENSE.txt` for details.

