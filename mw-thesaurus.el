;;; mw-thesaurus.el --- Merriam-Webster Thesaurus -*- lexical-binding: t; -*-
;;
;; Filename: mw-thesaurus.el
;; Description: Thesaurus look up through www.dictionaryapi.com.
;; Author: Ag Ibragimov
;; Maintainer: Ag Ibragimov (concat "agzam.ibragimov" "@" "gm" "ail" ".c" "om")
;; Copyright (C) 2017  Ag Ibragimov

;; Keywords: synonyms thesaurus dictionary
;; Version: 0.0.1

;;; Commentary:

;; Thesaurus look up through www.dictionaryapi.com - Merriam-Webster online dictionary
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For more than 150 years, in print and now online, Merriam-Webster has been America's leading and most-trusted provider of language information.
;; Each month, Merriam-Webster web sites offer guidance to more than 40 million visitors. In print, publications include Merriam-Webster's Collegiate Dictionary (among the best-selling books in American history) and newly published dictionaries for English-language learners.
;; All Merriam-Webster products and services are backed by the largest team of professional dictionary editors and writers in America, and one of the largest in the world.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'request)
(require 'thingatpt)
(require 'xml)

(defcustom mw-thesaurus--api-key
  "67d977d5-790b-412e-a547-9dbcc2bcd525"
  "Merriam-Webster API access key")

(defcustom mw-thesaurus--base-url
  "http://www.dictionaryapi.com/api/v1/references/thesaurus/xml/"
  "Merriam-Webster API base URL")

(defun get-xml-node (root path)
  "From parsed XML retrieves a node

Usage: `(get-xml-node html-root '(html head title))`"
  (let* ((current-node (xml-get-children root (car path))))
    (if (< 1 (length path))
        (get-xml-node (car current-node) (cdr path))
      current-node)))

(defun mw-thesaurus--italicize (prop)
  "Checks if the element contains <it> tag, retrieves content, resulting string is placed between '/' and '/'"
  (let* ((its (get-xml-node prop '(it))))
    (mapconcat
     (lambda (e)
       (if (member e its)
           (concat "/" (-> e last car string-trim) "/")
         (when (stringp e ) e)))
     prop "")))

(defun mw-thesaurus--snd-subs (article)
  (let* ((whole-sub (-> article
                        (get-xml-node '(vi)) car))
         (sub-str (mw-thesaurus--italicize whole-sub)))
    (concat "   - " sub-str)))

(defun mw-thesaurus--other-tag (article tag-type)
  (let* ((content (-> article
                        (get-xml-node `(,tag-type))
                        car
                        mw-thesaurus--italicize))
         (title (cond
                 ((eq tag-type 'syn) "Synonyms")
                 ((eq tag-type 'rel) "Related words")
                 ((eq tag-type 'near) "Near antonyms")
                 ((eq tag-type 'ant) "Antonyms")
                 (t "Unknown type"))))
    (when (and content (< 0 (length content)))
      (string-join (list "\n*** " title ":\n    " (s-replace ";" "\n   " content)) ""))))

(defun mw-thesaurus--third-lvl (article)
  (let* ((syns (mw-thesaurus--other-tag article 'syn))
         (rels (mw-thesaurus--other-tag article 'rel))
         (nears (mw-thesaurus--other-tag article 'near))
         (ants (mw-thesaurus--other-tag article 'ant)))
    (string-join (list syns rels nears ants) "")))

(defun mw-thesaurus--snd-level (entry)
  (let ((articles (get-xml-node entry '(sens))))
    (mapconcat
     (lambda (article)
       (let* ((desc (-> (get-xml-node article '(mc))
                       car
                       mw-thesaurus--italicize))
              (snd-subs (mw-thesaurus--snd-subs article))
              (third-lvl (mw-thesaurus--third-lvl article)))
         (string-join (list "** " desc "\n" snd-subs third-lvl) "")))
     articles
     "\n")))

(defun mw-thesaurus--get-title (entry)
  (-> (get-xml-node entry '(term hw))
      car (seq-drop 2) car))

(defun mw-thesaurus--get-type (entry)
  (-> (get-xml-node entry '(fl))
      car (seq-drop 2) car))

(defun mw-thesaurus--parse (xml-data)
  "Parses XML returned by Merriam-Webster dictionary API,
returns multi-line text in org-mode format"
  (let* ((entry-list (assq 'entry_list xml-data))
         (entries (xml-get-children entry-list 'entry)))
      (mapconcat
       (lambda (entry)
         (let* ((fst-level (concat "* " (mw-thesaurus--get-title entry)
                                   " [" (mw-thesaurus--get-type entry) "]\n"))
                (snd-level (mw-thesaurus--snd-level entry)))
           (string-join (list fst-level snd-level) "")))
       entries "")))

(defun mw-thesaurus--create-buffer (word data)
  (let ((dict-str (mw-thesaurus--parse data)))
    (if (< (length dict-str) 1)
        (message (concat "Sadly, Merriam-Webster doesn't seem to have anything for " word))
      (let* ((buffer-name "* Thesaurus *")
             (temp-buf (get-buffer-create buffer-name)))
        (set-buffer temp-buf)
        (with-current-buffer temp-buf
          (setf (buffer-string) "")
          (funcall 'org-mode)
          (insert (decode-coding-string dict-str 'dos))
          (goto-char (point-min))
          (read-only-mode))
        (switch-to-buffer-other-window temp-buf)))))

(defun mw-thesaurus/lookup-at-point ()
  "looks up a thesaurus definition for word at point
using Merriam-Webster online dictionary"
  (interactive)
  (let* ((word (word-at-point))
         (url (concat (symbol-value 'mw-thesaurus--base-url)
                      word "?key="
                      (symbol-value 'mw-thesaurus--api-key))))
    (request url
     :parser (lambda () (xml-parse-region (point-min) (point-max)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (mw-thesaurus--create-buffer word data))))))

(provide 'mw-thesaurus)
