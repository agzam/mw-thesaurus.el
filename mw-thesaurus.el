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

(require 'request nil t)

(defcustom mw-thesaurus--api-key "67d977d5-790b-412e-a547-9dbcc2bcd525")

(defcustom mw-thesaurus--base-url "http://www.dictionaryapi.com/api/v1/references/thesaurus/xml/")

(defvar data
  (let* ((xml "
        <?xml version=\"1.0\" encoding=\"utf-8\"?>
        <entry_list version=\"1.0\">
          <entry id=\"umpire\">
            <term>
              <hw>umpire</hw>
            </term>
            <fl>noun</fl>
            <sens>
              <mc>a person who impartially decides or resolves a dispute or controversy</mc>
              <vi>usually acts as <it>umpire</it> in the <it> umpire-yo </it> all-too-frequent squabbles between the two other roommates</vi>
              <syn>adjudicator, arbiter, arbitrator, referee, umpire</syn>
              <rel>jurist, justice, magistrate; intermediary, intermediate, mediator, mediatrix, moderator, negotiator; conciliator, go-between, peacemaker, reconciler, troubleshooter; decider</rel>
            </sens>
          </entry>
          <entry id=\"umpire\">
            <term>
              <hw>umpire</hw>
            </term>
            <fl>verb</fl>
            <sens>
              <mc>to give an opinion about (something at issue or in dispute)</mc>
              <vi>in our family disputes regarding the use of our home entertainment system are <it>umpired</it> by Dad</vi>
              <syn>adjudge, adjudicate, arbitrate, decide, determine, referee, rule (on), settle, umpire</syn>
              <rel>consider, deem, deliberate, hear, ponder, weigh; size up; mediate, moderate, negotiate; prosecute, try; find (for <it>or</it> against); conclude, resolve; redetermine, rejudge</rel>
              <near>equivocate, hedge, pussyfoot, skirt</near>
            </sens>
          </entry>
        </entry_list>"))
      (with-temp-buffer
        (insert xml)
        (xml-parse-region (point-min) (point-max)))))

(defun mw-thesaurus/get-entires (tree)
  (let* ((entry-list (assq 'entry_list tree)))
      (xml-get-children entry-list 'entry)))

(defun get-xml-node (root path)
  (let* ((current-node (xml-get-children root (car path))))
    (if (< 1 (length path))
        (get-xml-node (car current-node) (cdr path))
      current-node)))

(defun mw-thesaurus/get-title (entry)
  (-> (get-xml-node entry '(term hw))
      car (seq-drop 2) car))

(defun mw-thesaurus/get-type (entry)
  (-> (get-xml-node entry '(fl))
      car (seq-drop 2) car))

(defun mw-thesaurus/snd-level (entry)
  (let* ((desc (-> (get-xml-node entry '(sens mc))
                   car (seq-drop 2) car)))
    (concat "** " desc)))

(defun mw-thesaurus/italisize (prop)
  (let* ((its      (get-xml-node prop '(it))))
    (mapconcat (lambda (e)
                 (if (member e its)
                     (concat "/" (-> e last car string-trim) "/")
                   (when (stringp e ) e))) prop "")))

(let* ((entry (car (mw-thesaurus/get-entires data)))
       (whole-sub (-> entry
                      (get-xml-node '(sens vi))
                      car
                      ;; car (seq-drop 2)
                      )))
  ;; whole-sub
  (mw-thesaurus/italisize whole-sub)
  ;; (get-xml-node whole-sub '(it))
  )

(defun mw-thesaurus/snd-subs (entry)
  (let* ((whole-sub (-> entry
                        (get-xml-node '(sens vi)) car))
         (sub-str (mw-thesaurus/italisize whole-sub)))
    (concat "   - " sub-str)))

(defun mw-thesaurus/other-tag (entry tag-type)
  (let* ((content (-> entry
                        (get-xml-node `(sens ,tag-type))
                        car
                        mw-thesaurus/italisize)))
    (print content)
    (when content
      (s-replace ";" "\n   " content))))

(defun mw-thesaurus/text ()
  (mapconcat
   (lambda (entry)
     (let* ((fst-level (concat "* " (mw-thesaurus/get-title entry)
                               " [" (mw-thesaurus/get-type entry) "]"))
            (snd-level (mw-thesaurus/snd-level entry))
            (snd-subs (mw-thesaurus/snd-subs entry))
            (syns (string-join (list "\n*** Synonyms:\n    " (mw-thesaurus/other-tag entry 'syn)) ""))
            (rels (string-join (list "\n*** Related words:\n    " (mw-thesaurus/other-tag entry 'rel)) ""))
            (nears (string-join (list "\n*** Near antonyms:\n    " (mw-thesaurus/other-tag entry 'near)) ""))
            (ants (string-join (list "\n*** Antonyms:\n    " (mw-thesaurus/other-tag entry 'ant)) "")))
       (string-join (list fst-level snd-level snd-subs
                          syns rels nears ants) "\n")))
   (mw-thesaurus/get-entires data) "\n\n"))


(string-join (list "**Syns: " (mw-thesaurus/other-tag (car (cdr (mw-thesaurus/get-entires data))) 'rel)) "")

(defun mw-thesaurus/lookup ()
  (let* ((org-entry )
         (temp-buf  (generate-new-buffer "* Thesaurus *")))
    (set-buffer temp-buf)
    (with-current-buffer temp-buf
      (funcall 'org-mode)
      (insert (mw-thesaurus/text)))
    (switch-to-buffer-other-window temp-buf)))

(mw-thesaurus/lookup)

(let* ((entry-list (assq 'entry_list data))
       (entries (xml-get-children entry-list 'entry)))
  (while entries
    (print (car entries))
    (print "\n")
    (setq entries (cdr entries))))


(provide 'mw-thesaurus)
