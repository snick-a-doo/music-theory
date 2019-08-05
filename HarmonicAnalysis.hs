-- Identify chord degree quality and inversion.
-- Copyright (C) 2019  Sam Varner  snick-a-doo@comcast.net
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module HarmonicAnalysis (analyze,
                         formatChord,
                         formatMeasure,
                         formatAnalysis) where

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import Test.HUnit

-- Utilities

-- Return the passed in list with repeated elements removed.
unique :: Eq t => [t] -> [t]
unique (x:y:ys)
  | x == y = unique (x:ys)
  | otherwise = x : unique (y:ys)
unique x = x

-- Return a list of differences between adjacent elements.
pairDiff :: Num t => [(t, t)] -> [(t, t)]
pairDiff (x:y:ys) = (fst y - fst x, snd y - snd x) : pairDiff (y:ys)
pairDiff _ = []

-- Given an alst with lists for keys, look up a list in the circularly extended
-- keys.  E.g. [1,2] matches [1,2,3], [3,1,2], and [2,3,1].  The last one
-- matches because it becomes [2,3,1.2.3] when circularly extended.  Return a
-- tuple of the value and the index where the key was matched. 
lookUpWrap :: Eq a => [a] -> [([a], b)] -> Maybe (b, Int)
lookUpWrap xs (t:ts) =
  let match = matchList 0 xs $ extend (fst t) in
  if fst match
  then Just (snd t, snd match)
  else lookUpWrap xs ts
  where
    extend xs = xs ++ xs
    matchList n xs lst@(y:ys) =
      if xs == take (length xs) lst
      then (True, n)
      else matchList (n+1) xs ys
    matchList n xs ys = (False, n)
lookUpWrap _ _ = Nothing

-- Types

type Interval = Int -- Number of half steps
type Degree = Int -- Degree of scale: C = 0, D = 1, etc.
type Natural = Char -- Capital A-G
type Sign = Char -- sharp (#) or flat (b)
type Note = String -- F#, Bb, C##, G, etc.
type NoteCode = (Degree, Interval) -- !! Change to data?
type Key = String -- "f#, Bb, etc. (lowercase for minor)
type ChordClass = String -- triad, 7th, etc.
type Quality = String -- MM7, m, etc.
type Inversion = Int -- 0 = root, 1 = 1st, etc.

data Analysis = Analysis {degree :: Degree,
                          quality :: Quality,
                          inversion :: Inversion}
              deriving (Eq, Show)

-- Constants

noteHalfSteps :: [(Natural, Interval)]
noteHalfSteps = zip "CDEFGAB" [0, 2, 4, 5, 7, 9, 11]

noteDegrees :: [(Natural, Degree)]
noteDegrees = zip "CDEFGAB" [0..]

noteSigns :: [(Sign, Interval)]
noteSigns = [('#', 1), ('b', -1)]

-- Look-up table for chord types and inversions.  Use lookUpWrap with a list of
-- differences in degree of the notes in the chord.
chordDegrees :: [([Degree], String)]
chordDegrees = [([2, 5],       "triad no 5th")
                ,([2, 2, 3],    "triad")
                ,([2, 4, 1],    "7th no 5th")
                ,([2, 2, 2, 1], "7th")]

chordClasses :: [(ChordClass, [([Interval], Quality)])]
chordClasses = [("triad no 5th", [([3, 9],       "m")
                                  ,([4, 8],       "M")])
                ,("triad",        [([3, 3, 6],    "o")
                                  ,([3, 4, 5],    "m")
                                  ,([4, 3, 5],    "M")])
                ,("7th no 5th",   [([3, 6, 3],    "o7")
                                  ,([3, 7, 2],    "m7")
                                  ,([3, 8, 1],    "mM7")
                                  ,([4, 6, 2],    "M7")
                                  ,([4, 7, 1],    "MM7")])
                ,("7th",          [([3, 3, 3, 3], "o7")
                                  ,([3, 4, 3, 2], "m7")
                                  ,([3, 3, 4, 2], "o/7")
                                  ,([3, 4, 4, 1], "mM7")
                                  ,([4, 3, 3, 2], "M7")
                                  ,([4, 3, 4, 1], "MM7")])]

-- Printed representation of inversions.
triadInversions = A.listArray (0, 2) ["", "6", "64"]
seventhInversions = A.listArray (0, 3) ["7", "65", "43", "42"]

-- Roman numerals for chord degrees.  Downcase for minor.
roman = A.listArray (0, 6) ["I", "II", "III", "IV", "V", "VI", "VII"]

-- Internal Functions

encodeNote :: Note -> Maybe NoteCode
encodeNote (natural:sharpsAndFlats) =
  L.lookup natural noteDegrees >>=
  (\degree -> L.lookup natural noteHalfSteps >>=
    (\halfs -> return (degree, halfs + modifier)))
  where
    modifier = sum $ M.mapMaybe signHalfStep sharpsAndFlats
    signHalfStep sign = L.lookup sign noteSigns

intervals :: [NoteCode] -> [NoteCode]
intervals = pairDiff . unique . L.sort

-- Shift encoded notes by octaves so the 1st note is the lowest.  Shifted notes
-- have interval > 11 and degree > 6.
raiseNotes :: [NoteCode] -> [NoteCode]
raiseNotes [] = []
raiseNotes notes@(n:ns) =
  map (raiseAbove n) notes
  where
    raiseAbove low note@(deg, int) =
      if note < low then (deg+7, int+12) else note

chordQuality :: [NoteCode] -> Maybe (Quality, Inversion)
chordQuality [] = Nothing
chordQuality notes =
  lookUpWrap degrees chordDegrees >>=
  (\(cClass, inversion) -> L.lookup cClass chordClasses >>=
    (\qAList -> lookUpWrap intervals qAList >>=
      (\(qual, _) -> return (qual, inversion))))
  where
    (degrees, intervals) = L.unzip notes

noteDegree :: Natural -> Maybe Degree
noteDegree natural = L.lookup natural noteDegrees

chordDegree :: Key -> [Note] -> Inversion -> Maybe Degree
chordDegree key chord inv =
  (noteDegree . C.toUpper . head $ key) >>=
  (\keyDeg -> (noteDegree . head . head $ chord) >>=
    (\noteDeg -> return $ mod (noteDeg - keyDeg - 2*inv) 7))

-- Interface

analyze :: Key -> String -> Maybe Analysis
analyze key chordString =
  (chordQuality . pairDiff . unique . L.sort . raiseNotes) chordCode >>=
  (\(qual, inv) -> chordDegree key chord inv >>=
    (\deg -> return $ Analysis deg qual inv))
  where
    chord = L.words chordString
    chordCode = M.mapMaybe encodeNote chord

formatChord :: Maybe Analysis -> String
-- Show a place holder for unrecognized chords.
formatChord Nothing = "--"
formatChord anl =
  degStr ++ dimStr ++ invStr
  where
    theAnl = M.fromMaybe (Analysis 0 "m" 0) anl
    degStr = (if major then id else map C.toLower) $ roman ! (degree theAnl)
    dimStr = if diminished then L.dropWhileEnd (\x -> x == '7')  qual  else ""
    invStr = (if seventh then seventhInversions else triadInversions) ! (inversion theAnl)
    major = head qual == 'M'
    diminished = head qual == 'o'
    seventh = last qual  == '7'
    qual = quality $ theAnl

formatMeasure :: Key -> [String] -> String
formatMeasure key measure = L.unwords $ map (formatChord . (analyze key)) measure

formatAnalysis :: Key -> [[String]] -> String
formatAnalysis key measures =
  key ++ ": " ++ L.intercalate "|" (map (formatMeasure key) measures)

-- Tests

haTests =
  test ["unique" ~: "one" ~: [2] ~=? (unique [2])
       ,"unique" ~: "two different" ~: [2,3] ~=? (unique [2,3])
       ,"unique" ~: "two same" ~: [2] ~=? (unique [2,2])
       ,"unique" ~: "four" ~: [2,3] ~=? (unique [2,2,2,3])
       ,"pairDiff" ~: "one" ~: [] ~=? (pairDiff [(2,3)])
       ,"pairDiff" ~: "two different" ~: [(1,3)] ~=? (pairDiff [(2,1),(3,4)])
       ,"pairDiff" ~: "3 different" ~: [(3,0),(-2,2)] ~=? (pairDiff [(2,1),(5,1),(3,3)])
       ,"pairDiff" ~: "two same" ~: [(0,0)] ~=? (pairDiff [(2,4),(2,4)])
       ,"lookUpWrap" ~: "front" ~: Just ("triad", 0) ~=? (lookUpWrap [2,2,3] chordDegrees)
       ,"lookUpWrap" ~: "middle" ~: Just ("7th", 2) ~=? (lookUpWrap [2,1,2,2] chordDegrees)
       ,"lookUpWrap" ~: "end" ~: Just ("triad no 5th", 1) ~=? (lookUpWrap [5,2] chordDegrees)
       ,"encodeNote" ~: "C" ~: [(0,0),(2,4),(4,7)] ~=? (M.mapMaybe encodeNote ["C","E","G"])
       ,"encodeNote" ~: "enharmonic" ~: [(0,2),(1,2),(2,2)] ~=? (M.mapMaybe encodeNote ["C##","D","Ebb"])
       ,"intervals" ~: "C" ~: [(2,4),(2,3)] ~=? (intervals [(0,0),(2,4),(4,7)])
       ,"chordDegree" ~: "DinC" ~: Just 1 ~=? (chordDegree "C" ["D","F#","A"] 0)
       ,"chordDegree" ~: "a#inF#" ~: Just 2 ~=? (chordDegree "F#" ["E#","A#","C#"] 2)
       ,"raiseNotes" ~: "A" ~: [(5,9),(7,13),(9,15)] ~=? (raiseNotes [(5,9),(0,1),(2,3)])

       ,"inversions" ~: "root" ~: Just (Analysis 0 "MM7" 0) ~=? (analyze "C" "C E G B")
       ,"inversions" ~: "1" ~: Just (Analysis 0 "MM7" 1) ~=? (analyze "C" "E G B C")
       ,"inversions" ~: "2" ~: Just (Analysis 0 "MM7" 2) ~=? (analyze "C" "G B C E")
       ,"inversions" ~: "3" ~: Just (Analysis 0 "MM7" 3) ~=? (analyze "C" "B C E G")

       ,"inversions" ~: "root" ~: Just (Analysis 0 "MM7" 0) ~=? (analyze "C" "C E G B C")
       ,"inversions" ~: "1" ~: Just (Analysis 0 "MM7" 1) ~=? (analyze "C" "E E G B C")
       ,"inversions" ~: "2" ~: Just (Analysis 0 "MM7" 2) ~=? (analyze "C" "G C E G B")
       ,"inversions" ~: "3" ~: Just (Analysis 0 "MM7" 3) ~=? (analyze "C" "B G B C E")

       ,"quality" ~: "mM7" ~: Just (Analysis 0 "mM7" 0) ~=? (analyze "C" "C Eb G B")
       ,"quality" ~: "m7" ~: Just (Analysis 0 "m7" 0) ~=? (analyze "C" "C Eb G Bb")
       ,"quality" ~: "o/7" ~: Just (Analysis 0 "o/7" 0) ~=? (analyze "C" "C Eb Gb Bb")
       ,"quality" ~: "o7" ~: Just (Analysis 0 "o7" 0) ~=? (analyze "C" "C# E G Bb")
       
       ,"degree" ~: "1" ~: Just (Analysis 1 "m7" 0) ~=? (analyze "C" "D F A C")
       ,"degree" ~: "2" ~: Just (Analysis 2 "m7" 0) ~=? (analyze "C" "E G B D")
       ,"degree" ~: "3" ~: Just (Analysis 3 "MM7" 0) ~=? (analyze "C" "F A C E")
       ,"degree" ~: "4" ~: Just (Analysis 4 "M7" 0) ~=? (analyze "C" "G B D F")
       ,"degree" ~: "5" ~: Just (Analysis 5 "m7" 0) ~=? (analyze "C" "A C E G")
       ,"degree" ~: "6" ~: Just (Analysis 6 "o/7" 0) ~=? (analyze "C" "B D F A")

       ,"key" ~: "D" ~: Just (Analysis 6 "MM7" 0) ~=? (analyze "D" "C E G B")
       ,"key" ~: "E" ~: Just (Analysis 5 "MM7" 0) ~=? (analyze "E" "C E G B")
       ,"key" ~: "F" ~: Just (Analysis 4 "MM7" 0) ~=? (analyze "F" "C E G B")
       ,"key" ~: "G" ~: Just (Analysis 3 "MM7" 0) ~=? (analyze "G" "C E G B")
       ,"key" ~: "A" ~: Just (Analysis 2 "MM7" 0) ~=? (analyze "A" "C E G B")
       ,"key" ~: "B" ~: Just (Analysis 1 "MM7" 0) ~=? (analyze "B" "C E G B")

       ,"triads" ~: "0" ~: Just (Analysis 0 "M" 0) ~=? (analyze "C" "C E G")
       ,"triads" ~: "1" ~: Just (Analysis 1 "m" 0) ~=? (analyze "C" "D F A")
       ,"triads" ~: "2" ~: Just (Analysis 2 "m" 1) ~=? (analyze "Eb" "Bb D G")
       ,"triads" ~: "3" ~: Just (Analysis 3 "M" 2) ~=? (analyze "Eb" "Eb C Ab")
       ,"triads" ~: "6" ~: Just (Analysis 6 "o" 2) ~=? (analyze "C" "F B D")

       ,"format" ~: "I" ~: "I" ~=? (formatChord $ analyze "F#" "F# A# F# C#")
       ,"format" ~: "V43" ~: "V43" ~=? (formatChord $ analyze "F#" "G# B E# C#")
       ,"format" ~: "I6" ~: "I6" ~=? (formatChord $ analyze "F#" "A# A# F# C#")
       ,"format" ~: "V42" ~: "V42" ~=? (formatChord $ analyze "F#" "B G# E# C#")
       
       ,"format" ~: "measure" ~: "I V43 I6 V42"
        ~=? (formatMeasure "F#" ["F# A# F# C#", "G# B E# C#", "A# A# F# C#", "B G# E# C#"])

       ,"format" ~: "analysis 1" ~: "C: I IV64 I|IV6 IV ii6|I64 V|I V64 I6|IV ii|I64 V|I"
        ~=? (formatAnalysis "C" [["C G E C", "C A F C", "C G E C"],
                                 ["A F F C", "F A F C", "F A F D"],
                                 ["G G E C", "G G D B"],
                                 ["C G E C", "D G D B", "E G C C"],
                                 ["F A F C", "D A F D"],
                                 ["G G E C", "G G D B"],
                                 ["C G E C"]])
       ,"format" ~: "analysis 2" ~: "F#: I V43 I6 V42|I6 IV7 V|I V65 IV6 viio/42|I64 V7 I"
        ~=? (formatAnalysis "F#" [["F# A# F# C#", "G# B E# C#", "A# A# F# C#", "B G# E# C#"],
                                ["A# F# F# C#", "B A# F# D#", "C# G# E# C#"],
                                ["F# A# F# C#", "E# B G# C#", "D# B F# B", "D# B E# G#"],
                                ["C# C# F# A#", "C# B E# G#", "F# A# C# F#"]])
       ,"format" ~: "analysis 3" ~: "b: i|V43 i6 iio/65 i64|viio42 V7 i|viio43 i6 V43 i|iv V7 i"
        ~=? (formatAnalysis "b" [["B B F# D"],
                                 ["C# A# F# E", "D B F# D", "E B G C#", "F# B F# D"],
                                 ["G A# E C#", "F# A# E C#", "B B D B"],
                                 ["E G C# A#", "D F# D B", "C# F# E A#", "B F# D B"],
                                 ["E G E B", "F# F# E A#", "B F# D B"]])
       ,"format" ~: "analysis 4" ~: "Bb: I|IV6 viio/7 I V43|I6 ii65 V|V42 I6 IV6 viio/42|I64 V7 I"
        ~=? (formatAnalysis "Bb" [["Bb Bb F D"],
                                  ["G Bb Eb Eb", "A G Eb C", "Bb F D Bb", "C F Eb A"],
                                  ["D F D Bb", "Eb G C Bb", "F F C A"],
                                  ["Eb A F C", "D Bb F Bb", "G Bb Eb Bb", "G C Eb A"],
                                  ["F D F Bb", "F C Eb A", "Bb Bb D Bb"]])
       ,"format" ~: "analysis 5" ~: "a: i|viio65 i6 viio43 i64|viio42 i64 V iv6|viio42 V7 VI iio/65|i64 V7 i"
        ~=? (formatAnalysis "a" [["A C E A"],
                                 ["B D F G#", "C C E A", "D B F G#", "E C E A"],
                                 ["F B D G#", "E C E A", "E B E G#", "F A D A"],
                                 ["F B D G#", "E B D G#", "F A C A", "D B F A"],
                                 ["E C E A", "E B D G#", "A A C A"]])
       ]

-- runTestTT haTests

