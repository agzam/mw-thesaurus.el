;;; mw-thesaurus.el --- Merriam-Webster Thesaurus and Dictionary -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/mw-thesaurus.el
;; Created: Nov-2017
;; Keywords: wp, matching
;; License: GPL v3
;; Package-Requires: ((emacs "27.1"))
;; Version: 2.1.0
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

(defconst mw-thesaurus--tabs
  '(thesaurus "Thesaurus" dictionary "Dictionary" synonym-chooser "Synonym Chooser"
              spanish "Spanish" medical "Medical")
  "The tabs of the header line in order, each followed by its label.
The Synonym Chooser shows how the dictionary tells close synonyms apart.
Spanish and Medical show the Spanish-English and Medical dictionaries.")

(defvar mw-thesaurus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-open-at-point] #'mw-thesaurus-browse)
    ;; Evil takes plain digits for counts; a minor mode that binds M-digits,
    ;; such as winum, outranks this map and keeps them.
    (dotimes (n (/ (length mw-thesaurus--tabs) 2))
      (define-key map (kbd (format "M-%d" (1+ n))) #'mw-thesaurus-switch-to-tab))
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
  "The tab of the page the buffer shows, one of `mw-thesaurus--tabs'.")

(defvar-local mw-thesaurus--no-entry nil
  "The other reference if it has no entry for the word of the page, else nil.")

(defvar-local mw-thesaurus--dictionary-entries nil
  "The dictionary's entries for the word of the page as (WORD . ENTRIES).
nil until the page asks the dictionary.")

(defvar-local mw-thesaurus--history nil
  "Pages the thesaurus buffer showed before the current one, newest first.
Each page is a list (WORD TEXT POINT REFERENCE NO-ENTRY DICTIONARY).")

;; Showing a page reruns `mw-thesaurus-mode', which kills buffer-local variables.
(put 'mw-thesaurus--word 'permanent-local t)
(put 'mw-thesaurus--text 'permanent-local t)
(put 'mw-thesaurus--reference 'permanent-local t)
(put 'mw-thesaurus--no-entry 'permanent-local t)
(put 'mw-thesaurus--dictionary-entries 'permanent-local t)
(put 'mw-thesaurus--history 'permanent-local t)

(defvar mw-thesaurus--lookups 0
  "The count of lookups started; a request carries its lookup's count.
A later lookup, a tab switch, going back and quitting raise it, so an
answer that comes after one of them is dropped.")

(defvar mw-thesaurus--header-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'mw-thesaurus-switch-reference)
    map)
  "Keymap of the labels of the other tabs in the header line.")

(defun mw-thesaurus--page-tabs ()
  "Return the tabs the page shows, in the order of `mw-thesaurus--tabs'.
It leaves out `mw-thesaurus--no-entry', and Spanish or Medical without
its key; a page without a dictionary entry also leaves out the Synonym
Chooser, which shows the dictionary's answer."
  (seq-difference (seq-filter #'symbolp mw-thesaurus--tabs)
                  (append (seq-remove #'mw-thesaurus--key '(spanish medical))
                          (if (eq mw-thesaurus--no-entry 'dictionary)
                              '(synonym-chooser dictionary)
                            (list mw-thesaurus--no-entry)))))

(defun mw-thesaurus--header-line ()
  "Return the header line: the tabs of the page, the one shown in bold.
A click on another label shows its tab, which the label carries."
  (concat " "
          (mapconcat
           (lambda (tab)
             (let ((label (plist-get mw-thesaurus--tabs tab)))
               (if (eq tab mw-thesaurus--reference)
                   (propertize label 'face 'bold)
                 (propertize label
                             'face 'shadow
                             'mouse-face 'header-line-highlight
                             'help-echo (format "mouse-1, M-%d: show the %s tab"
                                                (1+ (seq-position
                                                     (seq-filter #'symbolp mw-thesaurus--tabs) tab))
                                                label)
                             'mw-thesaurus-tab tab
                             'local-map mw-thesaurus--header-line-map))))
           (mw-thesaurus--page-tabs) "   ")))

(defun mw-thesaurus--clicked-tab (event)
  "Return the tab of the header-line label that EVENT clicked, or nil."
  (when-let* (((mouse-event-p event))
              (string (posn-string (event-start event))))
    (get-text-property (cdr string) 'mw-thesaurus-tab (car string))))

(defun mw-thesaurus--read-only-keys ()
  "Return `mw-thesaurus-read-only-map' less the keys bound in the mode map.
The package binds none of those keys in `mw-thesaurus-mode-map', so one
bound there is the user's, and it acts as it did when that map held every
key."
  (let ((map (copy-keymap mw-thesaurus-read-only-map)))
    (map-keymap-internal
     (lambda (event binding)
       (if (and (eq event 'remap) (keymapp binding))
           (map-keymap-internal
            (lambda (command _) (define-key map (vector 'remap command) nil))
            binding)
         (define-key map (vector event) nil)))
     mw-thesaurus-mode-map)
    map))

(define-derived-mode mw-thesaurus-mode org-mode "Merriam-Webster"
  "Major mode for Merriam-Webster thesaurus entries, derived from Org mode.
\\{mw-thesaurus-mode-map}
On a read-only page:
\\{mw-thesaurus-read-only-map}"
  :group 'mw-thesaurus
  (setq-local org-hide-emphasis-markers t)
  ;; `org-startup-truncated' cuts lines off, and a paragraph is one line.
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; As in `diff-mode', single keys act only while the page is read-only.
  (setq-local minor-mode-overriding-map-alist
              (cons (cons 'buffer-read-only (mw-thesaurus--read-only-keys))
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

(defcustom mw-thesaurus-spanish-api-key nil
  "Merriam-Webster Spanish-English Dictionary API access key.
The header line shows the Spanish tab only while this is set."
  :type '(choice (const :tag "None" nil) string))

(defcustom mw-thesaurus-medical-api-key nil
  "Merriam-Webster Medical Dictionary API access key.
The header line shows the Medical tab only while this is set."
  :type '(choice (const :tag "None" nil) string))

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

(defconst mw-thesaurus--spanish-api-url
  "https://www.dictionaryapi.com/api/v3/references/spanish/json/"
  "Merriam-Webster Spanish-English Dictionary API URL.")

(defconst mw-thesaurus--medical-api-url
  "https://www.dictionaryapi.com/api/v3/references/medical/json/"
  "Merriam-Webster Medical Dictionary API URL.")

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
  "{\\(?:sx\\|dxt\\)|\\([^|}:]*\\)\\(?::\\([^|}]*\\)\\)?|[^|}]*|\\(\\(?:[^|{}]\\|{/?it}\\)*\\)}"
  "A cross-reference to a dictionary entry: its word, entry number and sense.
The sense may hold italics, as in {dxt|be||{it}auxiliary verb{/it} 5}.")

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

(defun mw-thesaurus--join (separator strings)
  "Join the non-nil STRINGS with SEPARATOR; return nil if none is left.
STRINGS comes last, so the call can end a `thread-last'."
  (when-let* ((strings (delq nil strings)))
    (string-join strings separator)))

(defun mw-thesaurus--genders (dt)
  "Return DT with each gender label or form joined to the translation before it.
The text after one joins too, so the translations share one line, as in
\"salvadoreño masculine, salvadoreña feminine\"."
  (reverse
   (seq-reduce
    (lambda (parts pair)
      (pcase-let ((`(,part . ,previous) pair))
        (if-let* ((text (and (equal "text" (caar parts)) (cadar parts)))
                  (joined (pcase part
                            (`("gl" ,label)
                             (concat (string-trim-right text) " {it}" label "{/it}"))
                            (`("gwds" ,form) (concat text (alist-get 'gwd form)))
                            ((and `("text" ,more)
                                  (guard (member (car previous) '("gl" "gwds"))))
                             (concat text more)))))
            (cons (list "text" joined) (cdr parts))
          (cons part parts))))
    ;; Each part with the one before it.
    (seq-mapn #'cons dt (cons nil dt))
    nil)))

(defun mw-thesaurus--dictionary-dt (dt)
  "Return the parts of DT, a definition text, as (KIND . TEXT) pairs.
KIND is `text' or `example'.  Usage notes and notes become text.  An
example ends with its translation, if it has one."
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
                                 (when-let* ((translation (alist-get 'tr vis)))
                                   (concat " — " (mw-thesaurus--dictionary-text translation)))
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
   (mw-thesaurus--genders dt)))

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

(defun mw-thesaurus--variants (variants)
  "Return the VARIANTS of a headword in Org bold, or nil if there are none.
A sense of the Spanish-English Dictionary names this way the phrase it
translates, as in \"or to set down\"."
  (thread-last variants
    (mapcar (lambda (variant)
              (concat (when-let* ((label (alist-get 'vl variant))) (concat label " "))
                      ;; A variant marks its syllables with * as a headword does.
                      "*" (replace-regexp-in-string
                           "\\*" "" (mw-thesaurus--dictionary-text (alist-get 'va variant)))
                      "*")))
    (mw-thesaurus--join ", ")))

(defun mw-thesaurus--dictionary-sense (sense base)
  "Format SENSE as Org list items BASE columns in, followed by its examples.
Each part of its number is an item: \"5 a\" is \"a.\" in an empty \"5.\".
The examples line up with the text of the last item."
  (let* ((parts (mw-thesaurus--dictionary-dt (alist-get 'dt sense)))
         (lead (when (eq 'text (caar parts)) (cdr (pop parts))))
         (labels (append (alist-get 'sls sense) (alist-get 'lbs sense)))
         (prefix (mw-thesaurus--join
                  " " (list (when labels (mw-thesaurus--italic (string-join labels ", ")))
                            (mw-thesaurus--variants (alist-get 'vrs sense)))))
         (text (if prefix
                   (mw-thesaurus--join " " (list prefix lead))
                 (when lead (replace-regexp-in-string "\\`\\(?:: *\\|— \\)" "" lead))))
         (items (mapcar (lambda (bullet)
                          (concat (make-string (+ base (* 3 (car bullet))) ?\s) (cdr bullet)))
                        (mw-thesaurus--sense-bullets (alist-get 'sn sense))))
         (body (make-string (if items (1+ (length (car (last items)))) base) ?\s))
         (divided (alist-get 'sdsense sense)))
    (concat (mapconcat (lambda (item) (concat item "\n")) (butlast items) "")
            (when-let* ((line (if items
                                  (mw-thesaurus--join " " (list (car (last items)) text))
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

(defun mw-thesaurus--dictionary-def (def &optional depth)
  "Format DEF, the senses under one verb divider, as an Org list.
DEPTH is that of the heading DEF goes under, 1 by default."
  (let* ((depth (or depth 1))
         (divider (alist-get 'vd def))
         (base (+ depth (if divider 2 1))))
    (concat (when divider (concat (make-string (1+ depth) ?*) " " divider "\n"))
            (when-let* ((labels (alist-get 'sls def)))
              (concat (make-string base ?\s) (mw-thesaurus--italic (string-join labels ", ")) "\n"))
            (mapconcat (lambda (item) (mw-thesaurus--dictionary-item item base))
                       (apply #'append (alist-get 'sseq def)) ""))))

(defun mw-thesaurus--spanish-p (entry)
  "Return non-nil if the headword of dictionary ENTRY is Spanish."
  (equal "es" (alist-get 'lang (alist-get 'meta entry))))

(defun mw-thesaurus--dictionary-pronunciation (language pronunciation)
  "Return PRONUNCIATION as text, or nil if it has no written form.
One with a recording is a link that plays it; LANGUAGE is \"es/\" for
the recording of a Spanish headword, else empty."
  ;; A sound one may leave out comes in italics; parentheses show it in a
  ;; link and in a terminal as well.
  (when-let* ((written (alist-get 'mw pronunciation))
              (sound (replace-regexp-in-string "{it}\\([^{]*\\){/it}" "(\\1)" written)))
    (concat (when-let* ((label (alist-get 'l pronunciation)))
              (mw-thesaurus--face (concat label " ") 'mw-thesaurus-pronunciation))
            (if-let* ((audio (alist-get 'audio (alist-get 'sound pronunciation))))
                (format "[[mw-thesaurus-audio:%s%s][%s]]" language audio sound)
              (mw-thesaurus--face sound 'mw-thesaurus-pronunciation)))))

(defun mw-thesaurus--dictionary-inflection (inflection)
  "Return INFLECTION as text, such as \"plural rodeos\", or nil without a form."
  (when-let* ((form (or (alist-get 'if inflection) (alist-get 'ifc inflection))))
    (concat (when-let* ((label (alist-get 'il inflection))) (concat label " "))
            (replace-regexp-in-string "\\*" "" form))))

(defun mw-thesaurus--dictionary-headlines (entry)
  "Return the syllables and pronunciations of ENTRY, then its forms, as lines.
A pronunciation with a recording is a link that plays it."
  (let ((headword (alist-get 'hw (alist-get 'hwi entry)))
        (language (if (mw-thesaurus--spanish-p entry) "es/" "")))
    (thread-last
      (list (thread-last (alist-get 'prs (alist-get 'hwi entry))
              (mapcar (apply-partially #'mw-thesaurus--dictionary-pronunciation language))
              (cons (when (string-match-p "\\*" headword)
                      (mw-thesaurus--face (replace-regexp-in-string "\\*" "·" headword)
                                          'mw-thesaurus-syllables)))
              (mw-thesaurus--join " "))
            (thread-last (alist-get 'ins entry)
              (mapcar #'mw-thesaurus--dictionary-inflection)
              (mw-thesaurus--join "; ")))
      (delq nil)
      (mapcar (lambda (line) (concat "  " line "\n")))
      (apply #'concat))))

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

(defun mw-thesaurus--dictionary-heading (entry)
  "Return the Org heading of ENTRY, a dictionary entry: headwords and function.
An alternate headword, such as a feminine form, follows the headword.  A
Spanish headword gets the tag Spanish."
  (let ((function (alist-get 'fl entry)))
    (concat "* "
            (mapconcat (lambda (hwi) (replace-regexp-in-string "\\*" "" (alist-get 'hw hwi)))
                       (cons (alist-get 'hwi entry) (alist-get 'ahws entry))
                       ", ")
            (when function (concat " ~" function "~"))
            (when (mw-thesaurus--spanish-p entry) " :Spanish:")
            "\n")))

(defun mw-thesaurus--dictionary-cognate (cognate)
  "Return COGNATE, a cross-reference such as \"past tense of go\", as a line."
  (concat "  " (alist-get 'cxl cognate) " "
          (mapconcat (lambda (target) (alist-get 'cxt target))
                     (alist-get 'cxtis cognate) ", ")
          "\n"))

(defun mw-thesaurus--dictionary-cross-references (xrs)
  "Return XRS, the cross-references of an entry such as \"ran\", as a line.
Return nil if there are none."
  (when-let* ((targets (delq nil (mapcar (lambda (target) (alist-get 'xrt target))
                                         (apply #'append xrs)))))
    (concat "  → "
            (mapconcat (lambda (target) (replace-regexp-in-string ":[0-9]+\\'" "" target))
                       targets ", ")
            "\n")))

(defun mw-thesaurus--dictionary-phrase (phrase)
  "Format PHRASE, a phrase with the headword such as \"hablarse\", as a subtree."
  (concat "** " (mw-thesaurus--dictionary-text (alist-get 'drp phrase)) "\n"
          (mapconcat (lambda (def) (mw-thesaurus--dictionary-def def 2))
                     (alist-get 'def phrase) "")))

(defun mw-thesaurus--dictionary-entry (entry)
  "Format ENTRY, a Collegiate or Spanish-English Dictionary entry, as a subtree."
  (concat (mw-thesaurus--dictionary-heading entry)
          (mw-thesaurus--dictionary-headlines entry)
          (mapconcat #'mw-thesaurus--dictionary-cognate (alist-get 'cxs entry) "")
          (mw-thesaurus--dictionary-cross-references (alist-get 'xrs entry))
          (mapconcat #'mw-thesaurus--dictionary-def (alist-get 'def entry) "")
          (mapconcat #'mw-thesaurus--dictionary-phrase (alist-get 'dros entry) "")
          (mw-thesaurus--dictionary-history entry)))

(defun mw-thesaurus--dictionary-parse (data)
  "Format the entries in DATA, a parsed dictionary response, as Org text."
  (mapconcat #'mw-thesaurus--dictionary-entry (seq-filter #'consp data) ""))

(defun mw-thesaurus--discussed (entries)
  "Return the dictionary ENTRIES that discuss their synonyms."
  (seq-filter (lambda (entry) (alist-get 'syns entry)) entries))

(defun mw-thesaurus--small-caps (part)
  "Return PART of a synonym discussion with its small capitals in italics.
A discussion sets the words it compares in small capitals; a run of them
becomes a list separated by commas."
  (if (equal "text" (car part))
      (list "text" (thread-last (cadr part)
                     (replace-regexp-in-string "{/sc} +{sc}" "{/sc}, {sc}")
                     (replace-regexp-in-string "{\\(/?\\)sc}" "{\\1it}")))
    part))

(defun mw-thesaurus--chooser-entry (entry)
  "Format the synonym discussions of the dictionary ENTRY as an Org subtree.
A sentence sits 2 columns in, its example 4."
  (concat (mw-thesaurus--dictionary-heading entry)
          (thread-last (alist-get 'syns entry)
            (seq-mapcat (lambda (discussion) (alist-get 'pt discussion)))
            (mapcar #'mw-thesaurus--small-caps)
            (mw-thesaurus--dictionary-dt)
            (mapcar (lambda (part)
                      (concat (if (eq 'text (car part)) "  " "    ") (cdr part) "\n")))
            (apply #'concat))))

(defun mw-thesaurus--chooser-parse (entries word)
  "Format the synonym discussions of the dictionary ENTRIES as Org text.
Without one, the page says that Merriam-Webster has none for WORD."
  (if-let* ((discussed (mw-thesaurus--discussed entries)))
      (mapconcat #'mw-thesaurus--chooser-entry discussed "")
    (format "* %s\n  Merriam-Webster doesn't explain how the synonyms of \"%s\" differ.\n"
            word word)))

(defun mw-thesaurus--audio-url (audio)
  "Return the URL of the Merriam-Webster recording named AUDIO.
The recording of a Spanish headword is named es/NAME."
  (let* ((spanish (string-prefix-p "es/" audio))
         (name (if spanish (substring audio 3) audio)))
    (format "https://media.merriam-webster.com/audio/prons/%s/mp3/%s/%s.mp3"
            (if spanish "es/me" "en/us")
            (cond ((string-prefix-p "bix" name) "bix")
                  ((string-prefix-p "gg" name) "gg")
                  ((string-match-p "\\`[^[:alpha:]]" name) "number")
                  (t (substring name 0 1)))
            name)))

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
                  (seq-some (lambda (stem)
                              (let ((stem (mw-thesaurus--normalize stem)))
                                ;; The Spanish-English Dictionary lists "give up" as "to give up".
                                (or (equal key stem)
                                    (equal key (string-remove-prefix "to " stem)))))
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
Return the body text unless it is a JSON array of entries or spelling
suggestions; a key error, for one, comes as plain text."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil 'move)
  (let* ((body (decode-coding-string
                (buffer-substring-no-properties (point) (point-max)) 'utf-8))
         (data (condition-case nil
                   (let ((json-array-type 'list))
                     (json-read-from-string body))
                 (json-error body))))
    (if (and (listp data)
             (seq-every-p (lambda (item) (or (stringp item) (consp (car-safe item))))
                          data))
        data
      (string-trim body))))

(defun mw-thesaurus--show (word text &optional pos reference no-entry dictionary)
  "Show TEXT, the entries for WORD in REFERENCE, in the current buffer.
Put point at POS, or at the start.  REFERENCE defaults to the thesaurus.
NO-ENTRY and DICTIONARY become the page's `mw-thesaurus--no-entry' and
`mw-thesaurus--dictionary-entries'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays (point-min) (point-max) 'mw-thesaurus t)
    (insert text))
  (mw-thesaurus-mode)
  (setq mw-thesaurus--word word
        mw-thesaurus--text text
        mw-thesaurus--reference (or reference 'thesaurus)
        mw-thesaurus--no-entry no-entry
        mw-thesaurus--dictionary-entries dictionary)
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

(defun mw-thesaurus--create-buffer (word dict-str &optional reference no-entry dictionary)
  "Show DICT-STR, the entries for WORD in REFERENCE, in the thesaurus buffer.
The page it replaces goes to the history of `mw-thesaurus-back'.
NO-ENTRY and DICTIONARY become the page's `mw-thesaurus--no-entry' and
`mw-thesaurus--dictionary-entries'; a nil DICTIONARY keeps those of WORD."
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
      (let ((dictionary (or dictionary
                            (and (equal word (car mw-thesaurus--dictionary-entries))
                                 mw-thesaurus--dictionary-entries))))
        (when mw-thesaurus--word
          (push (list mw-thesaurus--word mw-thesaurus--text (point) mw-thesaurus--reference
                      mw-thesaurus--no-entry mw-thesaurus--dictionary-entries)
                mw-thesaurus--history))
        (mw-thesaurus--show word dict-str nil reference no-entry dictionary)))))

(defun mw-thesaurus-back ()
  "Show the entries the thesaurus buffer had before the current ones.
Point returns to where it was, and no request goes to Merriam-Webster."
  (interactive)
  (unless mw-thesaurus--history
    (user-error "No earlier lookup to go back to"))
  ;; A lookup still waiting for its answer must not replace this page.
  (setq mw-thesaurus--lookups (1+ mw-thesaurus--lookups))
  (apply #'mw-thesaurus--show (pop mw-thesaurus--history)))

(defun mw-thesaurus--key-variable (reference)
  "Return the variable that holds the API key of REFERENCE."
  (pcase reference
    ('dictionary 'mw-thesaurus-dictionary-api-key)
    ('spanish 'mw-thesaurus-spanish-api-key)
    ('medical 'mw-thesaurus-medical-api-key)
    (_ 'mw-thesaurus-api-key)))

(defun mw-thesaurus--key-help (reference)
  "Return what to do about a missing or refused key for REFERENCE."
  (format "Set `%s' to a %s key from https://dictionaryapi.com"
          (mw-thesaurus--key-variable reference)
          (pcase reference
            ('dictionary "Collegiate Dictionary")
            ('spanish "Spanish-English Dictionary")
            ('medical "Medical Dictionary")
            (_ "Collegiate Thesaurus"))))

(defun mw-thesaurus--source (reference)
  "Return how the echo area names REFERENCE."
  (pcase reference
    ('dictionary "Merriam-Webster dictionary")
    ('spanish "Merriam-Webster Spanish-English")
    ('medical "Merriam-Webster medical dictionary")
    (_ "Merriam-Webster")))

(defun mw-thesaurus--other-reference (reference)
  "Return the reference that is not REFERENCE; nil stands for the thesaurus."
  (if (eq reference 'dictionary) 'thesaurus 'dictionary))

(defun mw-thesaurus--key (reference)
  "Return the API key of REFERENCE, or nil if it is blank."
  (let ((key (symbol-value (mw-thesaurus--key-variable reference))))
    (unless (string-blank-p (or key "")) key)))

(defun mw-thesaurus--request (word reference callback &optional lookup)
  "Ask REFERENCE about WORD, then call CALLBACK with the parsed answer.
A failed request, or a reply that is not JSON such as a key error, goes
to the echo area instead.  LOOKUP is the count of the lookup the request
serves; see `mw-thesaurus--lookups'."
  (let ((key (or (mw-thesaurus--key reference)
                 (user-error "%s" (mw-thesaurus--key-help reference))))
        answered)
    (url-retrieve
     (concat (pcase reference
               ('dictionary mw-thesaurus--dictionary-api-url)
               ('spanish mw-thesaurus--spanish-api-url)
               ('medical mw-thesaurus--medical-api-url)
               (_ mw-thesaurus--api-url))
             (url-hexify-string (mw-thesaurus--fold word)) "?key=" key)
     (lambda (status)
       (let ((response (current-buffer)))
         ;; CALLBACK runs in the response buffer: in a buffer of the
         ;; user's, such as *scratch*, [[:space:]] misses a newline.
         (unwind-protect
             ;; url.el calls back a second time for a refused connection.
             (unless (or answered (and lookup (/= lookup mw-thesaurus--lookups)))
               (setq answered t)
               (let* ((err (plist-get status :error))
                      (data (unless err (mw-thesaurus--read-response))))
                 (cond
                  (err (message "%s request failed: %S"
                                (mw-thesaurus--source reference) err))
                  ((stringp data)
                   (let ((reply (car (split-string data "\n"))))
                     (message "%s: %s%s" (mw-thesaurus--source reference) reply
                              (if (string-prefix-p "Invalid API key" reply)
                                  (concat "  " (mw-thesaurus--key-help reference))
                                ""))))
                  (t (funcall callback data)))))
           (kill-buffer response))))
     nil t t)))

(defun mw-thesaurus--show-entries (word data reference &optional no-entry)
  "Show the entries in DATA, REFERENCE's answer for WORD; nil if it has none.
NO-ENTRY becomes the page's `mw-thesaurus--no-entry'.  A dictionary page
keeps its entries for the Synonym Chooser."
  (when-let* ((entries (mw-thesaurus--own-entries data word)))
    (mw-thesaurus--create-buffer
     word
     ;; Spanish-English and medical entries have the layout of the dictionary's.
     (if (memq reference '(spanish medical dictionary))
         (mw-thesaurus--dictionary-parse entries)
       (mw-thesaurus--parse entries))
     reference no-entry (and (eq reference 'dictionary) (cons word entries)))
    t))

(defun mw-thesaurus--lookup-suggestion (word reference data fallback)
  "Look up the first spelling suggestion in DATA, REFERENCE's answer for WORD.
Take it only one or two edits away, at most one per two letters; else
show FALLBACK's entries for WORD."
  (let* ((suggestion (car (mw-thesaurus--suggestions data)))
         (edits (and suggestion
                     (string-distance (downcase word) (downcase suggestion)))))
    (if (and suggestion (< 0 edits 3) (<= (* 2 edits) (length word)))
        (mw-thesaurus--lookup-word suggestion reference word fallback)
      (mw-thesaurus--lookup-fallback word reference fallback))))

(defun mw-thesaurus--lookup-fallback (word reference fallback)
  "Show FALLBACK's entries for WORD, which REFERENCE has no entry for.
With no FALLBACK, no key for it, or no entry there either, say that
REFERENCE has none."
  (if (and fallback (mw-thesaurus--key fallback))
      (mw-thesaurus--request
       word fallback
       (lambda (answer)
         (unless (mw-thesaurus--show-entries word answer fallback reference)
           (message "No %s entry for \"%s\"" (mw-thesaurus--source reference) word)))
       mw-thesaurus--lookups)
    (message "No %s entry for \"%s\"" (mw-thesaurus--source reference) word)))

(defun mw-thesaurus--lookup-word (word &optional reference missed-word fallback)
  "Look up WORD in REFERENCE, the thesaurus by default, and show its entries.
With no entry, look up the closest spelling suggestion, and failing that,
show the entries for the word in FALLBACK, the other reference.
MISSED-WORD is the word whose suggestion WORD is."
  (let ((reference (or reference 'thesaurus))
        (lookup (setq mw-thesaurus--lookups (1+ mw-thesaurus--lookups))))
    (mw-thesaurus--request
     word reference
     (lambda (data)
       (cond
        ((mw-thesaurus--show-entries word data reference)
         (when missed-word
           (message "No %s entry for \"%s\", showing \"%s\""
                    (mw-thesaurus--source reference) missed-word word)))
        (missed-word (mw-thesaurus--lookup-fallback missed-word reference fallback))
        (t (mw-thesaurus--lookup-suggestion word reference data fallback))))
     lookup)))

(defun mw-thesaurus--show-tab (tab)
  "Show the page's dictionary entries in TAB, `dictionary' or `synonym-chooser'."
  (let ((entries (cdr mw-thesaurus--dictionary-entries)))
    (mw-thesaurus--create-buffer
     mw-thesaurus--word
     (if (eq tab 'dictionary)
         (mw-thesaurus--dictionary-parse entries)
       (mw-thesaurus--chooser-parse entries mw-thesaurus--word))
     tab mw-thesaurus--no-entry)))

(defun mw-thesaurus--lookup-chooser (word)
  "Ask the dictionary about WORD, the page's word, and show its Synonym Chooser.
The page keeps the answer, so its Dictionary tab needs no request."
  (mw-thesaurus--request
   word 'dictionary
   (lambda (data)
     (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
       (with-current-buffer buffer
         (setq mw-thesaurus--dictionary-entries
               (cons word (mw-thesaurus--own-entries data word)))
         (mw-thesaurus--show-tab 'synonym-chooser))))
   (setq mw-thesaurus--lookups (1+ mw-thesaurus--lookups))))

(defun mw-thesaurus-switch-reference (&optional tab)
  "Show the page's word in TAB, by default the next tab of the header line.
Interactively, TAB is that of the label clicked.  The Dictionary and the
Synonym Chooser show one answer, so a switch between them sends no
request."
  (interactive (list (mw-thesaurus--clicked-tab last-input-event)))
  (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                           (user-error "No Merriam-Webster page to switch from"))
    (let* ((tabs (mw-thesaurus--page-tabs))
           (tab (or tab (cadr (memq mw-thesaurus--reference (append tabs tabs))))))
      (cond
       ((memq tab (list nil mw-thesaurus--reference))
        (user-error "No %s entry for \"%s\""
                    (mw-thesaurus--source mw-thesaurus--no-entry) mw-thesaurus--word))
       ((and (eq tab 'synonym-chooser) (null mw-thesaurus--dictionary-entries))
        (mw-thesaurus--lookup-chooser mw-thesaurus--word))
       ((or (eq tab 'synonym-chooser)
            (and (eq tab 'dictionary) (cdr mw-thesaurus--dictionary-entries)))
        ;; A lookup still waiting for its answer must not replace the new page.
        (setq mw-thesaurus--lookups (1+ mw-thesaurus--lookups))
        (mw-thesaurus--show-tab tab))
       (t (mw-thesaurus--lookup-word mw-thesaurus--word tab))))))

(defun mw-thesaurus-switch-to-tab (n)
  "Show the page's word in tab N of the header line, counting from 1.
Interactively, N is the digit typed with Meta.  N names a tab of
`mw-thesaurus--tabs', so a hidden tab keeps its number and says why
it is hidden."
  (interactive (list (- (event-basic-type last-command-event) ?0)))
  (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                           (user-error "No Merriam-Webster page to switch from"))
    (let ((tab (nth (1- n) (seq-filter #'symbolp mw-thesaurus--tabs))))
      (cond ((eq tab mw-thesaurus--reference))
            ((memq tab (mw-thesaurus--page-tabs)) (mw-thesaurus-switch-reference tab))
            ((not (mw-thesaurus--key tab)) (user-error "%s" (mw-thesaurus--key-help tab)))
            (t (user-error "No %s entry for \"%s\""
                           (mw-thesaurus--source mw-thesaurus--no-entry) mw-thesaurus--word))))))

(defun mw-thesaurus--page-reference ()
  "Return the reference of the page: the dictionary for a Synonym Chooser."
  (if (eq mw-thesaurus--reference 'synonym-chooser) 'dictionary mw-thesaurus--reference))

(defun mw-thesaurus-browse ()
  "Open the page of the thesaurus buffer on merriam-webster.com.
A Spanish page opens on nglish.com, as merriam-webster.com has none.
On a link of the page, such as a pronunciation, follow the link instead."
  (interactive)
  (if (and (derived-mode-p 'mw-thesaurus-mode)
           (org-in-regexp org-link-bracket-re 1))
      (org-open-at-point)
    (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                             (user-error "No Merriam-Webster page to open"))
      ;; The syllable dots of a word looked up from a dictionary page.
      (let ((word (url-hexify-string (replace-regexp-in-string "·" "" mw-thesaurus--word))))
        (browse-url (if (eq mw-thesaurus--reference 'spanish)
                        (concat "https://www.nglish.com/spanish/en/" word)
                      (format "https://www.merriam-webster.com/%s/%s"
                              (mw-thesaurus--page-reference) word)))))))

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
        ;; On an empty line Emacs 27 and 28 give its newline as the word.
        ((not (string-blank-p (or (thing-at-point 'word t) "")))
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
  (interactive (list (point) (mw-thesaurus--page-reference)))
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
REFERENCE, the thesaurus by default, can also be `dictionary', `spanish'
or `medical'."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (mw-thesaurus--lookup-word (mw-thesaurus-get-original-word beginning end)
                             reference nil (mw-thesaurus--other-reference reference)))

(defun mw-thesaurus--quit ()
  "Quit the selected window and kill the Merriam-Webster buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    ;; A lookup still waiting for its answer must not bring the page back.
    (setq mw-thesaurus--lookups (1+ mw-thesaurus--lookups))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-thesaurus)

;;; mw-thesaurus.el ends here
