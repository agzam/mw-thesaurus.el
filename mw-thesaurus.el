;;; mw-thesaurus.el --- Merriam-Webster Thesaurus and Dictionary -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/mw-thesaurus.el
;; Created: Nov-2017
;; Keywords: wp, matching
;; License: GPL v3
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.4.0
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Look up a word in the Merriam-Webster thesaurus or Collegiate Dictionary
;; at dictionaryapi.com and read its entries in an Org buffer.
;; `mw-thesaurus-lookup-dwim' and `mw-thesaurus-dictionary-lookup-dwim' look
;; up the region, the word at point, or a word they ask for.

;;; Code:

(require 'url)
(require 'thingatpt)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'ucs-normalize)

(defgroup mw-thesaurus nil
  "Merriam-Webster Thesaurus and Dictionary."
  :prefix "mw-thesaurus-"
  :group 'applications)

(defvar mw-thesaurus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-open-at-point] #'mw-thesaurus-browse)
    map)
  "Keymap for `mw-thesaurus-mode'.
Single keys go in `mw-thesaurus-read-only-map'.")

(defvar mw-thesaurus-read-only-map
  (let ((map (make-sparse-keymap)))
    ;; Evil's state maps outrank this map; a remap reaches past them.
    (define-key map [remap evil-ret] #'mw-thesaurus-lookup-at-point)
    (define-key map [remap evil-record-macro] #'mw-thesaurus--quit)
    (define-key map [remap evil-window-top] #'mw-thesaurus-back)
    (define-key map [remap evil-open-below] #'mw-thesaurus-switch-reference)
    (define-key map [remap evil-org-open-below] #'mw-thesaurus-switch-reference)
    (define-key map (kbd "RET") #'mw-thesaurus-lookup-at-point)
    (define-key map (kbd "l") #'mw-thesaurus-back)
    (define-key map (kbd "o") #'mw-thesaurus-switch-reference)
    (define-key map (kbd "q") #'mw-thesaurus--quit)
    map)
  "Keymap of a read-only page in `mw-thesaurus-mode'.
It outranks `mw-thesaurus-mode-map'.  After \\[read-only-mode] the page
types these keys, and evil's keys work as in any buffer.")

(defvar mw-thesaurus-buffer-name "* Merriam-Webster *"
  "Name of the buffer that shows thesaurus and dictionary pages.")

(defvar-local mw-thesaurus--word nil
  "The word whose entries the thesaurus buffer shows.")

(defvar-local mw-thesaurus--text nil
  "The page the thesaurus buffer shows, as it came from the parser.
The buffer's own text can lose the indentation and faces of the page.")

(defvar-local mw-thesaurus--reference 'thesaurus
  "The reference of the page the buffer shows, `thesaurus' or `dictionary'.")

(defvar-local mw-thesaurus--history nil
  "Pages the thesaurus buffer showed before the current one, newest first.
Each page is a list (WORD TEXT POINT REFERENCE).")

;; Showing a page reruns `mw-thesaurus-mode', which kills buffer-local variables.
(put 'mw-thesaurus--word 'permanent-local t)
(put 'mw-thesaurus--text 'permanent-local t)
(put 'mw-thesaurus--reference 'permanent-local t)
(put 'mw-thesaurus--history 'permanent-local t)

(defvar mw-thesaurus--header-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'mw-thesaurus-switch-reference)
    map)
  "Keymap of the other reference's label in the header line.")

(defun mw-thesaurus--header-line ()
  "Return the header line: both references, the one shown in bold."
  (concat " "
          (mapconcat
           (lambda (reference)
             (let ((label (capitalize (symbol-name reference))))
               (if (eq reference mw-thesaurus--reference)
                   (propertize label 'face 'bold)
                 (propertize label
                             'face 'shadow
                             'mouse-face 'header-line-highlight
                             'help-echo (format "mouse-1: show the word in the %s" reference)
                             'local-map mw-thesaurus--header-line-map))))
           '(thesaurus dictionary) "   ")))

(define-derived-mode mw-thesaurus-mode org-mode "Merriam-Webster"
  "Major mode for Merriam-Webster thesaurus entries, derived from Org mode.
\\{mw-thesaurus-mode-map}
On a read-only page:
\\{mw-thesaurus-read-only-map}"
  :group 'mw-thesaurus
  (setq-local org-hide-emphasis-markers t)
  ;; As in `diff-mode', single keys act only while the page is read-only.
  (setq-local minor-mode-overriding-map-alist
              (cons (cons 'buffer-read-only mw-thesaurus-read-only-map)
                    minor-mode-overriding-map-alist))
  (setq-local header-line-format '(:eval (mw-thesaurus--header-line)))
  (setq buffer-read-only t))

(defcustom mw-thesaurus-api-key
  "67d977d5-790b-412e-a547-9dbcc2bcd525"
  "Merriam-Webster API access key."
  :type 'string)

(defcustom mw-thesaurus-dictionary-api-key
  "7dff2263-dfba-4c1b-8802-96422aa7bb49"
  "Merriam-Webster Collegiate Dictionary API access key."
  :type 'string)

(defcustom mw-thesaurus-audio-player nil
  "Command that plays a pronunciation: a program and its arguments.
The recording's file comes last.  nil means the first of afplay, mpv,
ffplay and mpg123 that `executable-find' finds."
  :type '(choice (const :tag "The first one found" nil) (repeat string)))

(defface mw-thesaurus-syllables '((t :inherit shadow))
  "Face of a dictionary headword split into syllables, such as de·cline.")

(defface mw-thesaurus-pronunciation '((t :inherit shadow))
  "Face of a pronunciation that has no recording to play, such as dē-.")

(defface mw-thesaurus-example '((t :inherit shadow))
  "Face of an example of a word in use under a dictionary sense.")

;; A package upgrade reloads this file; a defcustom would keep the old URL.
(defconst mw-thesaurus--api-url
  "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/"
  "Merriam-Webster thesaurus API URL.")

(defconst mw-thesaurus--dictionary-api-url
  "https://www.dictionaryapi.com/api/v3/references/collegiate/json/"
  "Merriam-Webster Collegiate Dictionary API URL.")

(defconst mw-thesaurus--word-lists
  '((syn_list . "Synonyms")
    (sim_list . "Synonyms & similar words")
    (rel_list . "Related words")
    (phrase_list . "Phrases")
    (near_list . "Near antonyms")
    (ant_list . "Antonyms")
    (opp_list . "Antonyms & near antonyms"))
  "Word lists of a sense in display order, with their headings.")

(defun mw-thesaurus--format-text (text)
  "Turn the Merriam-Webster markup in TEXT into Org markup."
  (string-trim
   (seq-reduce
    (lambda (acc rule)
      (replace-regexp-in-string (car rule) (cdr rule) acc t))
    ;; Zero-width spaces let Org see the emphasis inside a word.
    '(("{it}\\([^{]*\\){/it}" . "\u200b/\\1/\u200b")
      ("{[a-z_]+|\\([^|}]*\\)[^}]*}" . "\\1")
      ("{[^}]*}" . ""))
    text)))

(defun mw-thesaurus--senses (entry)
  "Return the senses of ENTRY, a parsed thesaurus entry."
  (thread-last (alist-get 'def entry)
    (seq-mapcat (lambda (def) (apply #'append (alist-get 'sseq def))))
    (seq-filter (lambda (item) (equal "sense" (car item))))
    (mapcar #'cadr)))

(defun mw-thesaurus--format-sense (sense)
  "Format SENSE as an Org subtree: meaning, examples and word lists."
  (let ((dt (alist-get 'dt sense)))
    (concat
     "** " (mw-thesaurus--format-text (or (cadr (assoc "text" dt)) "")) "\n"
     (mapconcat (lambda (vis)
                  (concat "   - " (mw-thesaurus--format-text (alist-get t vis)) "\n"))
                (cadr (assoc "vis" dt)) "")
     (mapconcat
      (lambda (word-list)
        (when-let* ((groups (alist-get (car word-list) sense)))
          (concat "*** " (cdr word-list) ":\n"
                  (mapconcat (lambda (group)
                               (concat "    "
                                       (mapconcat (lambda (word) (alist-get 'wd word))
                                                  group ", ")
                                       "\n"))
                             groups ""))))
      mw-thesaurus--word-lists ""))))

(defun mw-thesaurus--parse (data)
  "Format the entries in DATA, a parsed API response, as Org text.
An entry the Collegiate Dictionary also has gets a link that plays the
dictionary's recording of it."
  (mapconcat
   (lambda (entry)
     (let ((headword (alist-get 'hw (alist-get 'hwi entry)))
           (target (alist-get 'target (alist-get 'meta entry))))
       (concat "* " headword " ~" (alist-get 'fl entry) "~"
               (when (equal "collegiate" (alist-get 'tsrc target))
                 (format " [[mw-thesaurus-entry-audio:%s:%s][🔊]]"
                         (alist-get 'tuuid target) headword))
               "\n"
               (mapconcat #'mw-thesaurus--format-sense
                          (mw-thesaurus--senses entry) ""))))
   (seq-remove #'stringp data) ""))

(defconst mw-thesaurus--cross-reference-regexp
  "{\\(?:sx\\|dxt\\)|\\([^|}:]*\\)\\(?::\\([^|}]*\\)\\)?|[^|}]*|\\([^|}]*\\)}"
  "A cross-reference to a dictionary entry: its word, entry number and sense.")

(defun mw-thesaurus--cross-reference (token)
  "Return the text of TOKEN, a cross-reference such as {dxt|stoop:1||3b}."
  (save-match-data
    (string-match mw-thesaurus--cross-reference-regexp token)
    (let ((entry (match-string 2 token))
          (sense (match-string 3 token)))
      (concat (match-string 1 token)
              (when entry (concat " entry " entry))
              (unless (string-empty-p sense) (concat " sense " sense))))))

(defconst mw-thesaurus--dictionary-markup
  `(("{bc}" . ": ")
    ("{\\(?:wi\\|qword\\|phrase\\)}" . "{it}")
    ("{/\\(?:wi\\|qword\\|phrase\\)}" . "{/it}")
    ("{i_link|\\([^|}:]*\\)[^}]*}" . "{it}\\1{/it}")
    ("{ldquo}" . "“")
    ("{rdquo}" . "”")
    ("{gloss}" . "[")
    ("{/gloss}" . "]")
    ("{dx_def}" . "(")
    ("{/dx_def}" . ")")
    ("{\\(?:dx\\|dx_ety\\)}" . "— ")
    ("{ma}" . "— more at ")
    ("{ds|[^}]*}" . "")
    (,mw-thesaurus--cross-reference-regexp . mw-thesaurus--cross-reference)
    ("{[a-z_]+|\\([^|}:]*\\)[^}]*}" . "\\1"))
  "Collegiate Dictionary markup and what replaces it, in order.")

(defun mw-thesaurus--dictionary-text (text)
  "Turn the Collegiate Dictionary markup in TEXT into Org markup."
  (mw-thesaurus--format-text
   (seq-reduce (lambda (acc rule)
                 (replace-regexp-in-string (car rule) (cdr rule) acc t))
               mw-thesaurus--dictionary-markup text)))

(defun mw-thesaurus--italic (text)
  "Return TEXT in Org italics."
  (mw-thesaurus--format-text (concat "{it}" text "{/it}")))

(defun mw-thesaurus--face (text face)
  "Return TEXT marked to show in FACE once on the page, see `mw-thesaurus--faces'."
  (propertize text 'mw-thesaurus-face face))

(defun mw-thesaurus--no-colon (text)
  "Return TEXT without a leading colon, which would make an Org line fixed-width."
  (replace-regexp-in-string "\\`: *" "" text))

(defun mw-thesaurus--join (strings separator)
  "Join the non-nil STRINGS with SEPARATOR; return nil if none is left."
  (when-let* ((strings (delq nil strings)))
    (string-join strings separator)))

(defun mw-thesaurus--dictionary-dt (dt)
  "Return the parts of DT, a definition text, as (KIND . TEXT) pairs.
KIND is `text' or `example'.  Usage notes and notes become text."
  (seq-mapcat
   (lambda (part)
     (pcase part
       (`("text" ,text) (list (cons 'text (mw-thesaurus--dictionary-text text))))
       (`("t" ,text) (list (cons 'text (concat "Note: " (mw-thesaurus--dictionary-text text)))))
       (`("vis" ,examples)
        (mapcar (lambda (vis)
                  (cons 'example
                        (mw-thesaurus--face
                         (concat (mw-thesaurus--dictionary-text (alist-get t vis))
                                 (when-let* ((author (alist-get 'auth (alist-get 'aq vis))))
                                   (concat " — " author)))
                         'mw-thesaurus-example)))
                examples))
       (`("uns" ,notes)
        (seq-mapcat (lambda (note)
                      (mapcar (lambda (part)
                                (if (eq 'text (car part))
                                    (cons 'text (concat "— " (cdr part)))
                                  part))
                              (mw-thesaurus--dictionary-dt note)))
                    notes))
       (`("ca" ,called)
        (list (cons 'text (concat (alist-get 'intro called) " "
                                  (mapconcat (lambda (cat) (alist-get 'cat cat))
                                             (alist-get 'cats called) ", ")))))
       (`("snote" ,note) (mw-thesaurus--dictionary-dt note))))
   dt))

(defun mw-thesaurus--dictionary-lines (parts indent)
  "Return PARTS of a definition text as Org lines after INDENT."
  (mapconcat (lambda (part)
               (concat indent
                       (if (eq 'text (car part)) (mw-thesaurus--no-colon (cdr part)) (cdr part))
                       "\n"))
             parts ""))

(defun mw-thesaurus--sense-bullets (number)
  "Return the Org list bullets of a sense NUMBER such as \"1 a (1)\".
Each is (LEVEL . BULLET): \"1.\" at level 0, \"a.\" at 1, \"1)\" at 2."
  (mapcar (lambda (part)
            (cond ((string-match "\\`(\\(.+\\))\\'" part)
                   (cons 2 (concat (match-string 1 part) ")")))
                  ((string-match-p "\\`[[:alpha:]]" part) (cons 1 (concat part ".")))
                  (t (cons 0 (concat part ".")))))
          (and number (split-string number))))

(defun mw-thesaurus--dictionary-sense (sense base)
  "Format SENSE as Org list items BASE columns in, followed by its examples.
Each part of its number is an item: \"5 a\" is \"a.\" in an empty \"5.\".
The examples line up with the text of the last item."
  (let* ((parts (mw-thesaurus--dictionary-dt (alist-get 'dt sense)))
         (lead (when (eq 'text (caar parts)) (cdr (pop parts))))
         (labels (append (alist-get 'sls sense) (alist-get 'lbs sense)))
         (text (if labels
                   (mw-thesaurus--join
                    (list (mw-thesaurus--italic (string-join labels ", ")) lead) " ")
                 (when lead (replace-regexp-in-string "\\`\\(?:: *\\|— \\)" "" lead))))
         (items (mapcar (lambda (bullet)
                          (concat (make-string (+ base (* 3 (car bullet))) ?\s) (cdr bullet)))
                        (mw-thesaurus--sense-bullets (alist-get 'sn sense))))
         (body (make-string (if items (1+ (length (car (last items)))) base) ?\s))
         (divided (alist-get 'sdsense sense)))
    (concat (mapconcat (lambda (item) (concat item "\n")) (butlast items) "")
            (when-let* ((line (if items
                                  (mw-thesaurus--join (list (car (last items)) text) " ")
                                (when text (concat body text)))))
              (concat line "\n"))
            (mw-thesaurus--dictionary-lines parts body)
            (when divided
              (let* ((parts (mw-thesaurus--dictionary-dt (alist-get 'dt divided)))
                     (lead (when (eq 'text (caar parts)) (cdr (pop parts)))))
                (concat body (mw-thesaurus--italic (alist-get 'sd divided))
                        (when lead (concat " " lead)) "\n"
                        (mw-thesaurus--dictionary-lines parts body)))))))

(defun mw-thesaurus--dictionary-item (item base)
  "Format ITEM of a sense sequence as Org list items BASE columns in."
  (pcase item
    (`("pseq" ,items)
     (mapconcat (lambda (item) (mw-thesaurus--dictionary-item item base)) items ""))
    (`("bs" ,binding) (mw-thesaurus--dictionary-sense (alist-get 'sense binding) base))
    (`(,(or "sense" "sen") ,sense) (mw-thesaurus--dictionary-sense sense base))))

(defun mw-thesaurus--dictionary-def (def)
  "Format DEF, the senses under one verb divider, as an Org list."
  (let* ((divider (alist-get 'vd def))
         (base (if divider 3 2)))
    (concat (when divider (concat "** " divider "\n"))
            (when-let* ((labels (alist-get 'sls def)))
              (concat (make-string base ?\s) (mw-thesaurus--italic (string-join labels ", ")) "\n"))
            (mapconcat (lambda (item) (mw-thesaurus--dictionary-item item base))
                       (apply #'append (alist-get 'sseq def)) ""))))

(defun mw-thesaurus--dictionary-headlines (entry)
  "Return the syllables and pronunciations of ENTRY, then its forms, as lines.
A pronunciation with a recording is a link that plays it."
  (let ((headword (alist-get 'hw (alist-get 'hwi entry))))
    (mapconcat
     (lambda (line) (if line (concat "  " line "\n") ""))
     (list
      (mw-thesaurus--join
       (cons (when (string-match-p "\\*" headword)
               (mw-thesaurus--face (replace-regexp-in-string "\\*" "·" headword)
                                   'mw-thesaurus-syllables))
             (mapcar (lambda (pronunciation)
                       (when-let* ((sound (alist-get 'mw pronunciation)))
                         (concat (when-let* ((label (alist-get 'l pronunciation)))
                                   (mw-thesaurus--face (concat label " ")
                                                       'mw-thesaurus-pronunciation))
                                 (if-let* ((audio (alist-get 'audio (alist-get 'sound pronunciation))))
                                     (format "[[mw-thesaurus-audio:%s][%s]]" audio sound)
                                   (mw-thesaurus--face sound 'mw-thesaurus-pronunciation)))))
                     (alist-get 'prs (alist-get 'hwi entry))))
       " ")
      (mw-thesaurus--join
       (mapcar (lambda (inflection)
                 (when-let* ((form (or (alist-get 'if inflection)
                                       (alist-get 'ifc inflection))))
                   (concat (when-let* ((label (alist-get 'il inflection)))
                             (concat label " "))
                           (replace-regexp-in-string "\\*" "" form))))
               (alist-get 'ins entry))
       "; "))
     "")))

(defun mw-thesaurus--dictionary-history (entry)
  "Return the etymology and first known use of ENTRY as an Org subtree, or nil."
  (let ((etymology (alist-get 'et entry))
        (date (alist-get 'date entry)))
    (when (or etymology date)
      (concat "** Word history\n"
              (mapconcat
               (lambda (part)
                 (pcase part
                   (`("text" ,text)
                    (concat "   Etymology: "
                            (string-remove-prefix "— " (mw-thesaurus--dictionary-text text))
                            "\n"))
                   (`("et_snote" ,note)
                    (mw-thesaurus--dictionary-lines (mw-thesaurus--dictionary-dt note) "   "))))
               etymology "")
              (when date
                (concat "   First known use: " (mw-thesaurus--dictionary-text date) "\n"))))))

(defun mw-thesaurus--dictionary-entry (entry)
  "Format ENTRY, a parsed Collegiate Dictionary entry, as an Org subtree."
  (let ((function (alist-get 'fl entry)))
    (concat "* " (replace-regexp-in-string "\\*" "" (alist-get 'hw (alist-get 'hwi entry)))
            (when function (concat " ~" function "~"))
            "\n"
            (mw-thesaurus--dictionary-headlines entry)
            (mapconcat (lambda (cognate)
                         (concat "  " (alist-get 'cxl cognate) " "
                                 (mapconcat (lambda (target) (alist-get 'cxt target))
                                            (alist-get 'cxtis cognate) ", ")
                                 "\n"))
                       (alist-get 'cxs entry) "")
            (mapconcat #'mw-thesaurus--dictionary-def (alist-get 'def entry) "")
            (mw-thesaurus--dictionary-history entry))))

(defun mw-thesaurus--dictionary-parse (data)
  "Format the entries in DATA, a parsed dictionary response, as Org text."
  (mapconcat #'mw-thesaurus--dictionary-entry (seq-filter #'consp data) ""))

(defun mw-thesaurus--audio-url (audio)
  "Return the URL of the Merriam-Webster recording named AUDIO."
  (format "https://media.merriam-webster.com/audio/prons/en/us/mp3/%s/%s.mp3"
          (cond ((string-prefix-p "bix" audio) "bix")
                ((string-prefix-p "gg" audio) "gg")
                ((string-match-p "\\`[^[:alpha:]]" audio) "number")
                (t (substring audio 0 1)))
          audio))

(defun mw-thesaurus--audio-player ()
  "Return the command that plays a recording; signal an error if there is none."
  (or mw-thesaurus-audio-player
      (seq-find (lambda (command) (executable-find (car command)))
                '(("afplay")
                  ("mpv" "--no-video" "--really-quiet")
                  ("ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet")
                  ("mpg123" "-q")))
      (user-error "No audio player found; set `mw-thesaurus-audio-player'")))

(defun mw-thesaurus--play (audio &optional _)
  "Play the Merriam-Webster recording named AUDIO.
The first play downloads it under the variable `temporary-file-directory'."
  (let ((player (mw-thesaurus--audio-player))
        (file (expand-file-name (concat "mw-thesaurus/" audio ".mp3")
                                temporary-file-directory)))
    (if (file-exists-p file)
        (make-process :name "mw-thesaurus-audio" :noquery t
                      :command (append player (list file)))
      (url-retrieve
       (mw-thesaurus--audio-url audio)
       (lambda (status)
         (if-let* ((err (plist-get status :error)))
             (message "Merriam-Webster audio failed: %S" err)
           (goto-char (point-min))
           (re-search-forward "\r?\n\r?\n" nil 'move)
           (make-directory (file-name-directory file) t)
           (let ((coding-system-for-write 'no-conversion))
             (write-region (point) (point-max) file nil 'silent))
           (mw-thesaurus--play audio))
         (kill-buffer))
       nil t t))))

(org-link-set-parameters "mw-thesaurus-audio" :follow #'mw-thesaurus--play)

(defvar mw-thesaurus--entry-recordings (make-hash-table :test #'equal)
  "The recording of each Collegiate Dictionary entry asked about, by uuid.
`none' stands for an entry without one.")

(defun mw-thesaurus--entry-recording (data uuid)
  "Return the first recording of the entry UUID in DATA, a dictionary answer."
  (when-let* ((entry (seq-find (lambda (entry)
                                 (equal uuid (alist-get 'uuid (alist-get 'meta entry))))
                               (seq-filter #'consp data))))
    (seq-some (lambda (pronunciation) (alist-get 'audio (alist-get 'sound pronunciation)))
              (alist-get 'prs (alist-get 'hwi entry)))))

(defun mw-thesaurus--entry-audio (path &optional _)
  "Play the recording of the Collegiate Dictionary entry PATH names.
PATH is UUID:WORD.  The first play asks the dictionary about WORD."
  (mw-thesaurus--audio-player)
  (let ((uuid (substring path 0 (string-match ":" path)))
        (word (substring path (match-end 0))))
    (pcase (gethash uuid mw-thesaurus--entry-recordings)
      ('nil
       (mw-thesaurus--request
        word 'dictionary
        (lambda (data)
          (puthash uuid (or (mw-thesaurus--entry-recording data uuid) 'none)
                   mw-thesaurus--entry-recordings)
          (mw-thesaurus--entry-audio path))))
      ('none (message "No Merriam-Webster recording of \"%s\"" word))
      (audio (mw-thesaurus--play audio)))))

(org-link-set-parameters "mw-thesaurus-entry-audio" :follow #'mw-thesaurus--entry-audio)

(defun mw-thesaurus--svg-color (color &optional toward weight)
  "Return COLOR as SVG's #RRGGBB, or mixed with WEIGHT, 0 to 1, of TOWARD.
Emacs color names and #RRRRGGGGBBBB strings mean nothing to SVG."
  (apply #'format "#%02x%02x%02x"
         (seq-mapn (lambda (from to)
                     (round (+ from (* (or weight 0) (- to from))) 257))
                   (color-values color)
                   (color-values (or toward color)))))

(defun mw-thesaurus--audio-svg (text font)
  "Return an SVG button with TEXT and a speaker for FONT, a `font-info' vector.
Empty TEXT leaves the speaker alone.  The button is 4 pixels taller than
a line, and its text sits where Emacs puts the line's baseline in an
image with `:ascent' `center'."
  (let* ((char (default-font-width))
         (height (+ (aref font 3) 4))
         (baseline (/ (+ height (aref font 8) (- (aref font 9)) 1) 2))
         (color (face-foreground 'link nil 'default))
         (background (face-background 'default nil t))
         (ink (mw-thesaurus--svg-color color))
         (icon (* 0.6 (aref font 3)))
         (bare (string-empty-p text))
         (pad (* (if bare 0.4 0.5) height))
         (icon-x (+ pad (if bare 0 (* char (+ (string-width text) 0.6)))))
         (width (round (+ icon-x icon (* 0.4 height))))
         (middle (/ height 2.0))
         (svg (svg-create width height)))
    (svg-rectangle svg 0.5 0.5 (- width 1) (- height 1)
                   :rx middle
                   :stroke (mw-thesaurus--svg-color background color 0.5)
                   :fill (mw-thesaurus--svg-color background color 0.12))
    (unless bare
      (svg-text svg text :x pad :y baseline :fill ink
                :font-family (face-attribute 'default :family nil 'default)
                :font-size (aref font 2)))
    (svg-polygon svg (mapcar (lambda (point)
                               (cons (+ icon-x (* icon (car point)))
                                     (+ middle (* icon (cdr point)))))
                             '((0 . -0.17) (0.2 . -0.17) (0.45 . -0.45)
                               (0.45 . 0.45) (0.2 . 0.17) (0 . 0.17)))
                 :fill ink)
    ;; Two sound waves: arcs around the speaker's mouth.
    (dolist (radius (list (* 0.3 icon) (* 0.6 icon)))
      (let ((x (+ icon-x (* 0.4 icon) (* 0.64 radius)))
            (dy (* 0.77 radius)))
        (dom-append-child
         svg (dom-node 'path
                       `((d . ,(format "M %.1f %.1f A %.1f %.1f 0 0 1 %.1f %.1f"
                                       x (- middle dy) radius radius x (+ middle dy)))
                         (fill . "none") (stroke . ,ink)
                         (stroke-width . ,(max 1 (* 0.1 icon)))
                         (stroke-linecap . "round"))))))
    svg))

(defun mw-thesaurus--audio-buttons ()
  "Draw each pronunciation link of the page as a button with a speaker.
A link to a dictionary entry gets the speaker alone.  A terminal, or an
Emacs without SVG, shows the links as they are."
  ;; An Emacs built without a window system has no `font-info'.
  (when (and (display-graphic-p) (image-type-available-p 'svg) (fboundp 'font-info))
    (save-excursion
      (goto-char (point-min))
      (let ((font (font-info (face-font 'default))))
        (while (re-search-forward
                "\\[\\[\\(mw-thesaurus-\\(entry-\\)?audio:[^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
          (let ((link (concat "[[" (match-string-no-properties 1) "]]")))
            (make-button (match-beginning 0) (match-end 0)
                         'display (svg-image (mw-thesaurus--audio-svg
                                              (if (match-beginning 2)
                                                  ""
                                                (match-string-no-properties 3))
                                              font)
                                             ;; The SVG is drawn to the font's pixels.
                                             :scale 1 :ascent 'center)
                         'action (lambda (_) (org-link-open-from-string link))
                         ;; Emacs would underline the image as it does the link.
                         'face '(:underline nil)
                         'follow-link t
                         'help-echo "mouse-1, RET: hear it"
                         'mw-thesaurus t)))))))

(defun mw-thesaurus--faces ()
  "Show each run of page text marked by `mw-thesaurus--face' in its face.
An overlay's face merges with the faces Org gives the text."
  (let ((start (point-min)))
    (while (setq start (text-property-not-all start (point-max) 'mw-thesaurus-face nil))
      (let* ((end (next-single-property-change start 'mw-thesaurus-face nil (point-max)))
             (overlay (make-overlay start end)))
        (overlay-put overlay 'face (get-text-property start 'mw-thesaurus-face))
        (overlay-put overlay 'mw-thesaurus t)
        (setq start end)))))

(defun mw-thesaurus--fold (word)
  "Return WORD without diacritics; the API matches only plain letters."
  (concat (seq-remove (lambda (char)
                        (eq 'Mn (get-char-code-property char 'general-category)))
                      (ucs-normalize-NFD-string word))))

(defun mw-thesaurus--normalize (text)
  "Return TEXT folded and lowercase, words split by one space, no punctuation."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]]+" " "
    (replace-regexp-in-string
     "[^[:alnum:][:space:]]" ""
     (replace-regexp-in-string "-" " " (downcase (mw-thesaurus--fold text)))))))

(defun mw-thesaurus--own-entries (data word)
  "Return the entries in DATA for WORD or whose headword holds WORD.
Return nil if none is for WORD itself.  The API also sends entries
that only list a phrase with WORD, such as \"burn the candle at both
ends\" for \"both\"."
  (let* ((key (mw-thesaurus--normalize word))
         (entries (seq-filter #'consp data))
         (own-p (lambda (entry)
                  (seq-some (lambda (stem) (equal key (mw-thesaurus--normalize stem)))
                            (alist-get 'stems (alist-get 'meta entry))))))
    (when (seq-some own-p entries)
      (seq-filter
       (lambda (entry)
         (or (funcall own-p entry)
             (string-match-p
              (concat "\\(?:\\`\\| \\)" (regexp-quote key) "\\(?: \\|\\'\\)")
              (mw-thesaurus--normalize (alist-get 'hw (alist-get 'hwi entry))))))
       entries))))

(defun mw-thesaurus--suggestions (data)
  "Return the spelling suggestions in DATA, a response without entries."
  (seq-filter #'stringp data))

(defun mw-thesaurus--read-response ()
  "Parse the HTTP response in the current `url-retrieve' buffer.
Return the body text when it is not JSON, e.g. a key error."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil 'move)
  (let ((body (decode-coding-string
               (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
    (condition-case nil
        (let ((json-array-type 'list))
          (json-read-from-string body))
      (json-error (string-trim body)))))

(defun mw-thesaurus--show (word text &optional pos reference)
  "Show TEXT, the entries for WORD in REFERENCE, in the current buffer.
Put point at POS, or at the start.  REFERENCE defaults to the thesaurus."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays (point-min) (point-max) 'mw-thesaurus t)
    (insert text))
  (mw-thesaurus-mode)
  (setq mw-thesaurus--word word
        mw-thesaurus--text text
        mw-thesaurus--reference (or reference 'thesaurus))
  ;; `org-indent-mode' indents body lines itself, on top of any spaces.
  (when (bound-and-true-p org-indent-mode)
    (mw-thesaurus--unindent))
  (mw-thesaurus--audio-buttons)
  (mw-thesaurus--faces)
  (goto-char (or pos (point-min))))

(defun mw-thesaurus--unindent ()
  "Remove the spaces that line each body line up with its heading.
The rest of a line's indentation, which nests list items, stays."
  (let ((inhibit-read-only t)
        (base 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "\\(\\*+\\) ")
               (setq base (1+ (length (match-string 1)))))
              ((and (< 0 base) (looking-at (format " \\{1,%d\\}" base)))
               (replace-match "")))
        (forward-line 1)))))

(defun mw-thesaurus--create-buffer (word dict-str &optional reference)
  "Show DICT-STR, the entries for WORD in REFERENCE, in the thesaurus buffer.
The page it replaces goes to the history of `mw-thesaurus-back'."
  (let ((temp-buf (get-buffer-create mw-thesaurus-buffer-name)))
    ;; The `url-retrieve' callback runs in the response buffer, not the user's.
    (unless (eq 'mw-thesaurus-mode
                (buffer-local-value 'major-mode (window-buffer)))
      ;; Keys typed at an open prompt must stay in the prompt.
      (if (active-minibuffer-window)
          (display-buffer temp-buf t)
        (switch-to-buffer-other-window temp-buf)))
    (set-buffer temp-buf)
    (with-current-buffer temp-buf
      (when mw-thesaurus--word
        (push (list mw-thesaurus--word mw-thesaurus--text (point) mw-thesaurus--reference)
              mw-thesaurus--history))
      (mw-thesaurus--show word dict-str nil reference))))

(defun mw-thesaurus-back ()
  "Show the entries the thesaurus buffer had before the current ones.
Point returns to where it was, and no request goes to Merriam-Webster."
  (interactive)
  (unless mw-thesaurus--history
    (user-error "No earlier lookup to go back to"))
  (apply #'mw-thesaurus--show (pop mw-thesaurus--history)))

(defun mw-thesaurus--key-help (reference)
  "Return what to do about a missing or refused key for REFERENCE."
  (format "Set `%s' to a Collegiate %s key from https://dictionaryapi.com"
          (if (eq reference 'dictionary)
              'mw-thesaurus-dictionary-api-key
            'mw-thesaurus-api-key)
          (capitalize (symbol-name reference))))

(defun mw-thesaurus--source (reference)
  "Return how the echo area names REFERENCE."
  (if (eq reference 'dictionary) "Merriam-Webster dictionary" "Merriam-Webster"))

(defun mw-thesaurus--request (word reference callback)
  "Ask REFERENCE about WORD, then call CALLBACK with the parsed answer.
A failed request, or a reply that is not JSON such as a key error, goes
to the echo area instead."
  (let* ((dictionary (eq reference 'dictionary))
         (key (if dictionary mw-thesaurus-dictionary-api-key mw-thesaurus-api-key)))
    (when (string-empty-p (or key ""))
      (user-error "%s" (mw-thesaurus--key-help reference)))
    (url-retrieve
     (concat (if dictionary mw-thesaurus--dictionary-api-url mw-thesaurus--api-url)
             (url-hexify-string (mw-thesaurus--fold word)) "?key=" key)
     (lambda (status)
       (let* ((response (current-buffer))
              (err (plist-get status :error))
              (data (unless err (mw-thesaurus--read-response))))
         ;; CALLBACK runs in the response buffer: in a buffer of the
         ;; user's, such as *scratch*, [[:space:]] misses a newline.
         (unwind-protect
             (cond
              (err (message "%s request failed: %S"
                            (mw-thesaurus--source reference) err))
              ((stringp data)
               (let ((reply (car (split-string data "\n"))))
                 (message "%s: %s%s" (mw-thesaurus--source reference) reply
                          (if (string-prefix-p "Invalid API key" reply)
                              (concat "  " (mw-thesaurus--key-help reference))
                            ""))))
              (t (funcall callback data)))
           (kill-buffer response))))
     nil t t)))

(defun mw-thesaurus--lookup-word (word &optional reference missed-word)
  "Look up WORD in REFERENCE, the thesaurus by default, and show its entries.
With no entry, look up the first spelling suggestion if it is one or two
edits away, at most one per two letters.  MISSED-WORD, the word that had
no entry, goes to the echo area and prevents a second fallback."
  (let ((reference (or reference 'thesaurus)))
    (mw-thesaurus--request
     word reference
     (lambda (data)
       (let* ((entries (mw-thesaurus--own-entries data word))
              (suggestion (car (mw-thesaurus--suggestions data)))
              (edits (and suggestion
                          (string-distance (downcase word) (downcase suggestion)))))
         (cond
          (entries
           (mw-thesaurus--create-buffer
            word
            (if (eq reference 'dictionary)
                (mw-thesaurus--dictionary-parse entries)
              (mw-thesaurus--parse entries))
            reference)
           (when missed-word
             (message "No %s entry for \"%s\", showing \"%s\""
                      (mw-thesaurus--source reference) missed-word word)))
          ((and suggestion (not missed-word) (< 0 edits 3)
                (<= (* 2 edits) (length word)))
           (mw-thesaurus--lookup-word suggestion reference word))
          (t
           (message "No %s entry for \"%s\""
                    (mw-thesaurus--source reference) (or missed-word word)))))))))

(defun mw-thesaurus-switch-reference ()
  "Look up the word of the page in the other reference, thesaurus or dictionary."
  (interactive)
  (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                           (user-error "No Merriam-Webster page to switch from"))
    (mw-thesaurus--lookup-word mw-thesaurus--word
                               (if (eq mw-thesaurus--reference 'dictionary)
                                   'thesaurus
                                 'dictionary))))

(defun mw-thesaurus-browse ()
  "Open the page of the thesaurus buffer on merriam-webster.com.
On a link of the page, such as a pronunciation, follow the link instead."
  (interactive)
  (if (and (derived-mode-p 'mw-thesaurus-mode)
           (org-in-regexp org-link-bracket-re 1))
      (org-open-at-point)
    (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                             (user-error "No Merriam-Webster page to open"))
      ;; The syllable dots of a word looked up from a dictionary page.
      (browse-url (format "https://www.merriam-webster.com/%s/%s"
                          mw-thesaurus--reference
                          (url-hexify-string
                           (replace-regexp-in-string "·" "" mw-thesaurus--word)))))))

(defun mw-thesaurus-get-original-word (beginning end)
  "Get a word to look for from the user.
`BEGINNING' and `END' correspond to the selected text (if selected).
If presented, the selected text will be used.
Otherwise, user must provide additional information."
  (if (use-region-p)
      (buffer-substring-no-properties beginning end)
    (read-string "Word to look up: ")))

(defun mw-thesaurus-is-at-the-beginning-of-word (word-point)
  "Predicate to check whether `WORD-POINT' points to the beginning of the word."
  (save-excursion
    ;; If we are at the beginning of a word
    ;; this will take us to the beginning of the previous word.
    ;; Otherwise, this will take us to the beginning of the current word.
    (backward-word)
    ;; This will take us to the end of the previous word or to the end
    ;; of the current word depending on whether we were at the beginning
    ;; of a word.
    (forward-word)
    ;; Compare our original position with wherever we're now to
    ;; separate those two cases
    (< (point) word-point)))

(defun mw-thesaurus--dwim (reference)
  "Look up the region, the word at point or a word read from the user.
REFERENCE is `thesaurus' or `dictionary'."
  (cond ((use-region-p)
         (mw-thesaurus-lookup (region-beginning) (region-end) reference))
        ((thing-at-point 'word)
         (mw-thesaurus-lookup-at-point (point) reference))
        (t (mw-thesaurus-lookup nil nil reference))))

;;;###autoload
(defun mw-thesaurus-lookup-dwim ()
  "Look up a thesaurus definition using Merriam-Webster online dictionary.
If a region is selected use mw-thesaurus-lookup-word
if a thing at point is not empty use mw-thesaurus-lookup-word-at-point
otherwise as for word using mw-thesaurus-lookup-word"
  (interactive)
  (mw-thesaurus--dwim 'thesaurus))

;;;###autoload
(defun mw-thesaurus-dictionary-lookup-dwim ()
  "Look up a word in the Merriam-Webster Collegiate Dictionary.
Look up the region, the word at point, or a word read from the user."
  (interactive)
  (mw-thesaurus--dwim 'dictionary))

;;;###autoload
(defun mw-thesaurus-lookup-at-point (word-point &optional reference)
  "Look up the word at WORD-POINT in REFERENCE, the thesaurus by default.
Interactively, look it up in the reference of the page shown.  On a
page's link, such as a pronunciation, follow the link instead."
  (interactive (list (point) mw-thesaurus--reference))
  (save-mark-and-excursion
    (goto-char word-point)
    (if (and (derived-mode-p 'mw-thesaurus-mode)
             (org-in-regexp org-link-bracket-re 1))
        (org-open-at-point)
      (unless (mw-thesaurus-is-at-the-beginning-of-word word-point)
        (backward-word))
      (set-mark (point))
      (forward-word)
      (activate-mark)
      (mw-thesaurus-lookup (region-beginning) (region-end) reference))))

;;;###autoload
(defun mw-thesaurus-lookup (&optional beginning end reference)
  "Look up a thesaurus definition using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required.
REFERENCE, the thesaurus by default, can also be `dictionary'."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (mw-thesaurus--lookup-word (mw-thesaurus-get-original-word beginning end) reference))

(defun mw-thesaurus--quit ()
  "Quit the selected window and kill the Merriam-Webster buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-thesaurus)

;;; mw-thesaurus.el ends here
