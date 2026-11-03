;;; mw-thesaurus.el --- Merriam-Webster Thesaurus -*- lexical-binding: t; -*-
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

;; Look up a word in the Merriam-Webster thesaurus at dictionaryapi.com and
;; read its meanings, synonyms and antonyms in an Org buffer.
;; `mw-thesaurus-lookup-dwim' looks up the region, the word at point, or a
;; word it asks for.

;;; Code:

(require 'url)
(require 'thingatpt)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'ucs-normalize)

(defgroup mw-thesaurus nil
  "Merriam-Webster Thesaurus."
  :prefix "mw-thesaurus-"
  :group 'applications)

(defvar mw-thesaurus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-open-at-point] #'mw-thesaurus-lookup-at-point)
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

(defvar mw-thesaurus-buffer-name "* Merriam-Webster Thesaurus *"
  "Default buffer name for Merriam-Webster Thesaurus.")

(defvar-local mw-thesaurus--word nil
  "The word whose entries the thesaurus buffer shows.")

(defvar-local mw-thesaurus--reference 'thesaurus
  "The reference of the page the buffer shows, `thesaurus' or `dictionary'.")

(defvar-local mw-thesaurus--history nil
  "Pages the thesaurus buffer showed before the current one, newest first.
Each page is a list (WORD TEXT POINT REFERENCE).")

;; Showing a page reruns `mw-thesaurus-mode', which kills buffer-local variables.
(put 'mw-thesaurus--word 'permanent-local t)
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
  "Format the entries in DATA, a parsed API response, as Org text."
  (mapconcat
   (lambda (entry)
     (concat "* " (alist-get 'hw (alist-get 'hwi entry))
             " ~" (alist-get 'fl entry) "~\n"
             (mapconcat #'mw-thesaurus--format-sense
                        (mw-thesaurus--senses entry) "")))
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
                        (concat "- " (mw-thesaurus--dictionary-text (alist-get t vis))
                                (when-let* ((author (alist-get 'auth (alist-get 'aq vis))))
                                  (concat " — " author)))))
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

(defun mw-thesaurus--dictionary-sense (sense level)
  "Format SENSE as an Org heading of LEVEL, followed by its examples."
  (let* ((parts (mw-thesaurus--dictionary-dt (alist-get 'dt sense)))
         (lead (when (eq 'text (caar parts)) (cdr (pop parts))))
         (labels (append (alist-get 'sls sense) (alist-get 'lbs sense)))
         (number (mw-thesaurus--join
                  (list (alist-get 'sn sense)
                        (when labels (mw-thesaurus--italic (string-join labels ", "))))
                  " "))
         (indent (make-string (1+ level) ?\s))
         (divided (alist-get 'sdsense sense)))
    (concat (make-string level ?*) " "
            (if number
                (mw-thesaurus--join (list number lead) " ")
              (mw-thesaurus--no-colon (or lead "")))
            "\n"
            (mw-thesaurus--dictionary-lines parts indent)
            (when divided
              (let* ((parts (mw-thesaurus--dictionary-dt (alist-get 'dt divided)))
                     (lead (when (eq 'text (caar parts)) (cdr (pop parts)))))
                (concat indent (mw-thesaurus--italic (alist-get 'sd divided))
                        (when lead (concat " " lead)) "\n"
                        (mw-thesaurus--dictionary-lines parts indent)))))))

(defun mw-thesaurus--dictionary-item (item level)
  "Format ITEM of a sense sequence as Org headings of LEVEL."
  (pcase item
    (`("pseq" ,items)
     (mapconcat (lambda (item) (mw-thesaurus--dictionary-item item level)) items ""))
    (`("bs" ,binding) (mw-thesaurus--dictionary-sense (alist-get 'sense binding) level))
    (`(,(or "sense" "sen") ,sense) (mw-thesaurus--dictionary-sense sense level))))

(defun mw-thesaurus--dictionary-def (def)
  "Format DEF, the senses under one verb divider, as Org headings."
  (let* ((divider (alist-get 'vd def))
         (level (if divider 3 2)))
    (concat (when divider (concat "** " divider "\n"))
            (when-let* ((labels (alist-get 'sls def)))
              (concat (make-string level ?\s) (mw-thesaurus--italic (string-join labels ", ")) "\n"))
            ;; Each sense shows its number, so all senses share one level.
            (mapconcat (lambda (item) (mw-thesaurus--dictionary-item item level))
                       (apply #'append (alist-get 'sseq def)) ""))))

(defun mw-thesaurus--dictionary-headline (entry)
  "Return the syllables, pronunciations and inflections of ENTRY, or nil."
  (let ((headword (alist-get 'hw (alist-get 'hwi entry))))
    (mw-thesaurus--join
     (list (when (string-match-p "\\*" headword)
             (replace-regexp-in-string "\\*" "·" headword))
           (mw-thesaurus--join
            (mapcar (lambda (pronunciation)
                      (when-let* ((sound (alist-get 'mw pronunciation)))
                        (concat (when-let* ((label (alist-get 'l pronunciation)))
                                  (concat label " "))
                                sound)))
                    (alist-get 'prs (alist-get 'hwi entry)))
            ", ")
           (mw-thesaurus--join
            (mapcar (lambda (inflection)
                      (when-let* ((form (or (alist-get 'if inflection)
                                            (alist-get 'ifc inflection))))
                        (concat (when-let* ((label (alist-get 'il inflection)))
                                  (concat label " "))
                                (replace-regexp-in-string "\\*" "" form))))
                    (alist-get 'ins entry))
            "; "))
     " | ")))

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
            (when-let* ((headline (mw-thesaurus--dictionary-headline entry)))
              (concat "  " headline "\n"))
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
    (insert text))
  (mw-thesaurus-mode)
  (setq mw-thesaurus--word word
        mw-thesaurus--reference (or reference 'thesaurus))
  (goto-char (or pos (point-min))))

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
        (push (list mw-thesaurus--word
                    (buffer-substring-no-properties (point-min) (point-max))
                    (point)
                    mw-thesaurus--reference)
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

(defun mw-thesaurus--lookup-word (word &optional reference missed-word)
  "Look up WORD in REFERENCE, the thesaurus by default, and show its entries.
With no entry, look up the first spelling suggestion if it is one or two
edits away, at most one per two letters.  MISSED-WORD, the word that had
no entry, goes to the echo area and prevents a second fallback."
  (let* ((reference (or reference 'thesaurus))
         (dictionary (eq reference 'dictionary))
         (key (if dictionary mw-thesaurus-dictionary-api-key mw-thesaurus-api-key))
         (source (if dictionary "Merriam-Webster dictionary" "Merriam-Webster")))
    (when (string-empty-p (or key ""))
      (user-error "%s" (mw-thesaurus--key-help reference)))
    (url-retrieve
     (concat (if dictionary mw-thesaurus--dictionary-api-url mw-thesaurus--api-url)
             (url-hexify-string (mw-thesaurus--fold word)) "?key=" key)
     (lambda (status)
       (let* ((err (plist-get status :error))
              (data (unless err (mw-thesaurus--read-response)))
              (entries (and (listp data) (mw-thesaurus--own-entries data word)))
              (suggestion (and (listp data) (car (mw-thesaurus--suggestions data))))
              (edits (and suggestion
                          (string-distance (downcase word) (downcase suggestion)))))
         (kill-buffer)
         (cond
          (err (message "%s request failed: %S" source err))
          ((stringp data)
           (let ((reply (car (split-string data "\n"))))
             (message "%s: %s%s" source reply
                      (if (string-prefix-p "Invalid API key" reply)
                          (concat "  " (mw-thesaurus--key-help reference))
                        ""))))
          (entries
           (mw-thesaurus--create-buffer
            word
            (if dictionary (mw-thesaurus--dictionary-parse entries) (mw-thesaurus--parse entries))
            reference)
           (when missed-word
             (message "No %s entry for \"%s\", showing \"%s\"" source missed-word word)))
          ((and suggestion (not missed-word) (< 0 edits 3)
                (<= (* 2 edits) (length word)))
           (mw-thesaurus--lookup-word suggestion reference word))
          (t
           (message "No %s entry for \"%s\"" source (or missed-word word))))))
     nil t t)))

(defun mw-thesaurus-switch-reference ()
  "Look up the word of the page in the other reference, thesaurus or dictionary."
  (interactive)
  (with-current-buffer (or (get-buffer mw-thesaurus-buffer-name)
                           (user-error "No Merriam-Webster page to switch from"))
    (mw-thesaurus--lookup-word mw-thesaurus--word
                               (if (eq mw-thesaurus--reference 'dictionary)
                                   'thesaurus
                                 'dictionary))))

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
Interactively, look it up in the reference of the page shown."
  (interactive (list (point) mw-thesaurus--reference))
  (save-mark-and-excursion
    (unless (mw-thesaurus-is-at-the-beginning-of-word word-point)
      (backward-word))
    (set-mark (point))
    (forward-word)
    (activate-mark)
    (mw-thesaurus-lookup (region-beginning) (region-end) reference)))

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
  "Kill Merriam-Webster Thesaurus buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-thesaurus)

;;; mw-thesaurus.el ends here
