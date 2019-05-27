;;; satb.el --- Format and play short soprano, alto, tenor, bass arrangements.

;; Copyright (C) 2019 Sam Varner <snick-a-doo@comcast.net>

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

;;; Commentary

;; Satb-mode allows convenient entry of short four-part arrangements.  Satb-mode
;; is derived from org-mode.  The parts are entered in an org-mode table.  For
;; example
;;   bes major
;;   2/4
;;   | d'4 bes8 a8  | g4 ees8 f8 | f2 | f    |
;;   | f4 f         | ees ees    | c2 | d    |
;;   | bes4 bes     | bes bes    | a2 | bes  |
;;   | bes,4 d      | ees g      | f2 | bes, |
;;
;; The command satb-show formats the arrangement with Lillypond and displays the
;; PDF output.  Satb-play plays the MIDI file produced by Lillypond.  Timidity
;; is required for MIDI playback.
;;
;; Multiple arrangements may be put under different org-mode headlines.
;; Satb-show formats the arrangement in the section that point is in.  Satb-play
;; plays the MIDI file produced from the last time satb-show was called.

;;; Code

(require 'subr-x)
(require 'lilypond-mode)

(defvar satb-midi-instrument "acoustic grand")

(defun satb->lilypond (title key time soprano alto tenor bass)
  "Make a string of Lillypond input from the arguments.
KEY is a note name followed by 'major' or 'minor'.  TIME is the time
signature.  SOPRANO, ALTO, TENOR, and BASS are the notes for the respective
parts."
  (let ((key (string-join (split-string key) " \\")))
    (format "\\version \"2.18.2\"
\\header {
  title = \"%s\"
}
\\score {
  \\layout {}
  \\midi {}
  \\relative c' {
    \\new ChoirStaff <<
      \\new Staff \\with {midiInstrument = #\"%s\"} {
        \\key %s \\time %s
        << {
          %s
        } \\\\ {
          %s
        } >>
      }
      \\new Staff \\with {midiInstrument = #\"%s\"} {
        \\key %s \\clef \"bass\"
        << {
          %s
        } \\\\ {
          %s
        } >>
      }
    >>
  }
}"
            title
            satb-midi-instrument key time soprano alto
            satb-midi-instrument key tenor bass)))

(defun satb-get-section ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "\\*")
      (org-previous-visible-heading 1))
    (re-search-forward "[^\\*]")
    (let ((start (point)))
      (org-next-visible-heading 1)
      (split-string (buffer-substring-no-properties
                     start
                     (point))
                    "\n"))))

(defun satb-parse ()
  "Return a list of strings for constructing a Lillypond file."
  (interactive)
  (let ((lines '()))
    (dolist (line-in (satb-get-section))
      (let ((line (car (split-string line-in "#"))))
        (if (not (string-match "^[[:space:]]*$" line))
            (setq lines (cons line lines)))))
    (message "%s" (car lines))
    (reverse lines)))

(defun satb-file (ext)
  "Return the current buffer's file name with the extension changed to EXT."
  (concat (file-name-sans-extension (buffer-file-name))
          "."
          ext))

(defun satb-old-p (ext)
  "Non-nil if the file with the extension EXT is older than the current file.
An error is given if the current-buffer's file doesn't have an
'satb' extension."
  (let ((source (buffer-file-name)))
    (unless (string= (file-name-extension source) "satb")
      (error "The current file, %s, is not an SATB file" source))
    (let ((source-attr (file-attributes source))
          (dest-attr (file-attributes (satb-file ext))))
      (or (not dest-attr)
          (time-less-p (nth 5 dest-attr)
                       (nth 5 source-attr))))))

(defun satb-process ()
  "Produce the Lillypond file from the SATB file in the current section."
  (let ((lines (satb-parse)))
    ;; Generate the Lillypond file
    (find-file (satb-file "ly"))
    (delete-region (point-min) (point-max))
    (insert (apply 'satb->lilypond lines)))
  (save-buffer)
  (LilyPond-command-lilypond)
  (bury-buffer))

(defun satb-show ()
  "Process the arrangement in the current section and show the result."
  (interactive)
  (save-buffer)
  (satb-process)
  (let ((pdf (satb-file "pdf")))
    (other-window 1)
    (find-file pdf)
    (auto-revert-mode 1)
    (other-window -1)))

(defun satb-play ()
  "Play the MIDI file produced by processing the current file."
  (interactive)
  (if (satb-old-p "midi")
      (satb-process))
  (start-process "SATB MIDI"
                 nil ; Don't show an output buffer for the command.
                 "timidity"
                 (satb-file "midi")))

(define-derived-mode satb-mode
  org-mode "SATB"
  "Mode for SATB arrangements"
  (define-key satb-mode-map (kbd "C-c C-s") 'satb-show)
  (define-key satb-mode-map (kbd "C-c C-p") 'satb-play))

(provide 'satb)
;;; satb.el ends here
