;;; mw-thesaurus-test.el --- Tests for mw-thesaurus -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'mw-thesaurus)

(defvar mw-thesaurus--word-not-exist-xml
  "
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<entry_list version=\"1.0\">
  <suggestion>jab</suggestion>
  <suggestion>table</suggestion>
  <suggestion>ad-lib</suggestion>
</entry_list>")

(ert-deftest mw-thesaurus--parse-test ()
  (let* ((xml (with-temp-buffer
                (insert-file-contents "./assets/sample.xml")
                (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml))
         (expected-org-text (string-trim (with-temp-buffer
                                           (insert-file-contents "./assets/sample.org")
                                           (buffer-string)))))
    (string= expected-org-text parsed-org-text)))

(ert-deftest mw-thesaurus--parse-not-existing-word-test ()
  (let* ((xml (with-temp-buffer
                (insert mw-thesaurus--word-not-exist-xml)
                (xml-parse-region (point-min) (point-max))))
         (parsed-org-text (mw-thesaurus--parse xml)))
    (should (equal "" parsed-org-text))))

(ert-deftest mw-thesaurus--correct-number-of-entries-test ()
    (let* ((xml (with-temp-buffer
               (insert-file-contents "./assets/sample.xml")
               (xml-parse-region (point-min) (point-max))))
        (entry-list (assq 'entry_list xml))
        (entries (xml-get-children entry-list 'entry)))
      (should (equal 2 (length entries)))))

(defun mw-thesaurus-test--response (word words suggestions)
  "Return the body the thesaurus API sends for WORD.
Only WORDS have entries.  Any other word gets SUGGESTIONS, or the plain
text \"Results not found\" when SUGGESTIONS is nil."
  (cond
   ((member word words)
    (format "<entry_list><entry><term><hw>%s</hw></term><fl>verb</fl>
<sens><mc>meaning</mc></sens></entry></entry_list>" word))
   (suggestions
    (format "<entry_list>%s</entry_list>"
            (mapconcat (lambda (s) (format "<suggestion>%s</suggestion>" s))
                       suggestions "")))
   (t "Results not found")))

(defun mw-thesaurus-test--lookup (words suggestions lookup-fn)
  "Call LOOKUP-FN against a fake thesaurus API made of WORDS and SUGGESTIONS.
Return a plist of the request URLs, the texts sent to the thesaurus
buffer and the echo area messages."
  (let (urls rendered messages)
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest settings)
                 (push url urls)
                 (string-match "/xml/\\([^?]*\\)" url)
                 (let ((body (mw-thesaurus-test--response
                              (url-unhex-string (match-string 1 url))
                              words suggestions)))
                   (funcall (plist-get settings :success)
                            :data (with-temp-buffer
                                    (insert body)
                                    (funcall (plist-get settings :parser)))))))
              ((symbol-function 'mw-thesaurus--create-buffer)
               (lambda (dict-str) (push dict-str rendered)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (funcall lookup-fn))
    (list :urls (nreverse urls)
          :rendered (nreverse rendered)
          :messages (nreverse messages))))

(defun mw-thesaurus-test--lookup-region (text words suggestions)
  "Select TEXT in a temporary buffer and call `mw-thesaurus-lookup' on it.
WORDS and SUGGESTIONS make the fake API, see `mw-thesaurus-test--lookup'."
  (with-temp-buffer
    (insert text)
    (let ((transient-mark-mode t))
      (set-mark (point-min))
      (mw-thesaurus-test--lookup
       words suggestions
       (lambda () (mw-thesaurus-lookup (region-beginning) (region-end)))))))

(ert-deftest mw-thesaurus-lookup-shows-first-suggestion-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "hapy" '("happy") '("happy" "hap"))))
    (should (equal 2 (length (plist-get result :urls))))
    (should (string-prefix-p "* happy ~verb~"
                             (car (plist-get result :rendered))))
    (should (equal '("No Merriam-Webster entry for \"hapy\", showing \"happy\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-without-suggestions-test ()
  (let ((result (mw-thesaurus-test--lookup-region "xqzvbnwpt" nil nil)))
    (should (equal 1 (length (plist-get result :urls))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"xqzvbnwpt\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-falls-back-only-once-test ()
  (let ((result (mw-thesaurus-test--lookup-region "hapy" nil '("happy"))))
    (should (equal 2 (length (plist-get result :urls))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"hapy\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-existing-word-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "happy" '("happy") '("hap"))))
    (should (equal 1 (length (plist-get result :urls))))
    (should (string-prefix-p "* happy ~verb~"
                             (car (plist-get result :rendered))))
    (should-not (plist-get result :messages))))

(ert-deftest mw-thesaurus-lookup-encodes-words-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "giv up" '("give up") '("give up" "give in"))))
    (should (string-match-p "/xml/giv%20up\\?key="
                            (nth 0 (plist-get result :urls))))
    (should (string-match-p "/xml/give%20up\\?key="
                            (nth 1 (plist-get result :urls))))
    (should (equal '("No Merriam-Webster entry for \"giv up\", showing \"give up\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-at-point-keeps-the-word-test ()
  (with-temp-buffer
    (insert "I am hapy today")
    (goto-char 8)
    (let* ((transient-mark-mode t)
           (result (mw-thesaurus-test--lookup
                    '("happy") '("happy")
                    (lambda () (mw-thesaurus-lookup-at-point (point))))))
      (should (equal "I am hapy today" (buffer-string)))
      (should (equal 8 (point)))
      (should (string-match-p "/xml/hapy\\?key="
                              (nth 0 (plist-get result :urls))))
      (should (equal '("No Merriam-Webster entry for \"hapy\", showing \"happy\"")
                     (plist-get result :messages))))))

(provide 'mw-thesaurus-test)
;;; mw-thesaurus-test.el ends here
