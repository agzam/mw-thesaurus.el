;;; mw-thesaurus.el --- Merriam-Webster Thesaurus -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/mw-thesaurus.el
;; Created: Nov-2017
;; Keywords: wp, matching
;; License: GPL v3
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.2.1
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
    ;; Evil's state maps outrank this map; a remap reaches past them.
    (define-key map [remap evil-ret] #'mw-thesaurus-lookup-at-point)
    (define-key map [remap evil-record-macro] #'mw-thesaurus--quit)
    (define-key map (kbd "RET") #'mw-thesaurus-lookup-at-point)
    (define-key map (kbd "q") #'mw-thesaurus--quit)
    map)
  "Keymap for `mw-thesaurus-mode'.")

(defvar mw-thesaurus-buffer-name "* Merriam-Webster Thesaurus *"
  "Default buffer name for Merriam-Webster Thesaurus.")

(define-derived-mode mw-thesaurus-mode org-mode "Merriam-Webster"
  "Major mode for Merriam-Webster thesaurus entries, derived from Org mode.
\\{mw-thesaurus-mode-map}"
  :group 'mw-thesaurus
  (setq-local org-hide-emphasis-markers t)
  (setq buffer-read-only t))

(defcustom mw-thesaurus-api-key
  "67d977d5-790b-412e-a547-9dbcc2bcd525"
  "Merriam-Webster API access key."
  :type 'string)

;; A package upgrade reloads this file; a defcustom would keep the old URL.
(defconst mw-thesaurus--api-url
  "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/"
  "Merriam-Webster thesaurus API URL.")

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

(defun mw-thesaurus--create-buffer (dict-str)
  "Show DICT-STR, the thesaurus entries in Org format, in the thesaurus buffer."
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
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert dict-str))
      (mw-thesaurus-mode)
      (goto-char (point-min)))))

(defun mw-thesaurus--lookup-word (word &optional missed-word)
  "Look up WORD and show its thesaurus entries.
With no entry, look up the first spelling suggestion if it is one or two
edits away, at most one per two letters.  MISSED-WORD, the word that had
no entry, goes to the echo area and prevents a second fallback."
  (url-retrieve
   (concat mw-thesaurus--api-url (url-hexify-string (mw-thesaurus--fold word))
           "?key=" mw-thesaurus-api-key)
   (lambda (status)
     (let* ((err (plist-get status :error))
            (data (unless err (mw-thesaurus--read-response)))
            (entries (and (listp data) (mw-thesaurus--own-entries data word)))
            (suggestion (and (listp data) (car (mw-thesaurus--suggestions data))))
            (edits (and suggestion
                        (string-distance (downcase word) (downcase suggestion)))))
       (kill-buffer)
       (cond
        (err (message "Merriam-Webster request failed: %S" err))
        ((stringp data) (message "Merriam-Webster: %s" (car (split-string data "\n"))))
        (entries
         (mw-thesaurus--create-buffer (mw-thesaurus--parse entries))
         (when missed-word
           (message "No Merriam-Webster entry for \"%s\", showing \"%s\""
                    missed-word word)))
        ((and suggestion (not missed-word) (< 0 edits 3)
              (<= (* 2 edits) (length word)))
         (mw-thesaurus--lookup-word suggestion word))
        (t
         (message "No Merriam-Webster entry for \"%s\""
                  (or missed-word word))))))
   nil t t))

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

;;;###autoload
(defun mw-thesaurus-lookup-dwim ()
  "Look up a thesaurus definition using Merriam-Webster online dictionary.
If a region is selected use mw-thesaurus-lookup-word
if a thing at point is not empty use mw-thesaurus-lookup-word-at-point
otherwise as for word using mw-thesaurus-lookup-word"
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (mw-thesaurus-lookup beg end))
      (if (thing-at-point 'word)
          (mw-thesaurus-lookup-at-point (point))
        (mw-thesaurus-lookup)))))

;;;###autoload
(defun mw-thesaurus-lookup-at-point (word-point)
  "Look up word at point WORD-POINT using Merriam-Webster online thesaurus."
  (interactive (list (point)))
  (save-mark-and-excursion
    (unless (mw-thesaurus-is-at-the-beginning-of-word word-point)
      (backward-word))
    (set-mark (point))
    (forward-word)
    (activate-mark)
    (mw-thesaurus-lookup (region-beginning) (region-end))))

;;;###autoload
(defun mw-thesaurus-lookup (&optional beginning end)
  "Look up a thesaurus definition using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (mw-thesaurus--lookup-word (mw-thesaurus-get-original-word beginning end)))

(defun mw-thesaurus--quit ()
  "Kill Merriam-Webster Thesaurus buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-thesaurus)

;;; mw-thesaurus.el ends here
