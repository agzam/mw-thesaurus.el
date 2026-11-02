;;; mw-thesaurus-test.el --- Tests for mw-thesaurus -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mw-thesaurus)

(defun mw-thesaurus-test--asset (file)
  "Return the text of FILE in ./assets, decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (expand-file-name file "assets")))
    (buffer-string)))

(defun mw-thesaurus-test--read (body)
  "Parse BODY the way `request' runs `mw-thesaurus--read-response'."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (mw-thesaurus--read-response)))

(ert-deftest mw-thesaurus--parse-test ()
  (should (equal (mw-thesaurus-test--asset "sample.org")
                 (mw-thesaurus--parse
                  (mw-thesaurus-test--read
                   (mw-thesaurus-test--asset "sample.json"))))))

(ert-deftest mw-thesaurus--parse-similar-words-test ()
  (let ((org (mw-thesaurus--parse
              (mw-thesaurus-test--read
               (mw-thesaurus-test--asset "sample.json")))))
    (should (string-match-p "^\\* decline (in) ~verb~$" org))
    (should (string-match-p
             "^\\*\\*\\* Synonyms & similar words:\n    abate, diminish (in), " org))
    (should (string-match-p
             "^\\*\\*\\* Antonyms & near antonyms:\n    build up, gain, " org))))

(ert-deftest mw-thesaurus--parse-not-existing-word-test ()
  (let ((data (mw-thesaurus-test--read "[\"jab\",\"table\",\"ad-lib\"]")))
    (should (equal "" (mw-thesaurus--parse data)))
    (should (equal '("jab" "table" "ad-lib") (mw-thesaurus--suggestions data)))))

(ert-deftest mw-thesaurus--format-text-test ()
  (should (equal "the \u200b/decline/\u200b of Rome"
                 (mw-thesaurus--format-text "the {it}decline{/it} of Rome ")))
  (should (equal "past tense of run"
                 (mw-thesaurus--format-text "past tense of {d_link|run|run}")))
  (should (equal "lacking strength"
                 (mw-thesaurus--format-text "{bc}lacking strength"))))

(defun mw-thesaurus-test--response (word words suggestions)
  "Return the body the thesaurus API sends for WORD.
Only WORDS have entries.  Any other word gets SUGGESTIONS."
  (if (member word words)
      (format "[{\"meta\":{\"id\":%s},\"hwi\":{\"hw\":%s},\"fl\":\"verb\",
\"def\":[{\"sseq\":[[[\"sense\",{\"dt\":[[\"text\",\"meaning\"]]}]]]}]}]"
              (json-encode word) (json-encode word))
    (json-encode (vconcat suggestions))))

(defun mw-thesaurus-test--call (respond lookup-fn)
  "Call LOOKUP-FN against a fake thesaurus API.
RESPOND takes the requested word and returns the response body, or an
error for the :error callback.  Return a plist of the request URLs, the
texts sent to the thesaurus buffer and the echo area messages."
  (let (urls rendered messages)
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest settings)
                 (push url urls)
                 (string-match "/json/\\([^?]*\\)" url)
                 (let ((body (funcall respond
                                      (url-unhex-string (match-string 1 url)))))
                   (if (stringp body)
                       (funcall (plist-get settings :success)
                                :data (with-temp-buffer
                                        (insert body)
                                        (goto-char (point-min))
                                        (funcall (plist-get settings :parser))))
                     (funcall (plist-get settings :error) :error-thrown body)))))
              ((symbol-function 'mw-thesaurus--create-buffer)
               (lambda (dict-str) (push dict-str rendered)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (funcall lookup-fn))
    (list :urls (nreverse urls)
          :rendered (nreverse rendered)
          :messages (nreverse messages))))

(defun mw-thesaurus-test--lookup (words suggestions lookup-fn)
  "Call LOOKUP-FN against a fake API where only WORDS have entries.
Other words get SUGGESTIONS.  See `mw-thesaurus-test--call'."
  (mw-thesaurus-test--call
   (lambda (word) (mw-thesaurus-test--response word words suggestions))
   lookup-fn))

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

(ert-deftest mw-thesaurus-lookup-accepts-two-edits-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "recieve" '("receive") '("receive" "relieve"))))
    (should (equal 2 (length (plist-get result :urls))))
    (should (string-prefix-p "* receive ~verb~"
                             (car (plist-get result :rendered))))))

(ert-deftest mw-thesaurus-lookup-ignores-distant-suggestion-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "weakest" '("recast") '("recast" "latest"))))
    (should (equal 1 (length (plist-get result :urls))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"weakest\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-ignores-the-same-word-test ()
  (let ((result (mw-thesaurus-test--lookup-region
                 "Tenacity" nil '("tenacity" "tenacious"))))
    (should (equal 1 (length (plist-get result :urls))))
    (should (equal '("No Merriam-Webster entry for \"Tenacity\"")
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
    (should (string-match-p "/json/giv%20up\\?key="
                            (nth 0 (plist-get result :urls))))
    (should (string-match-p "/json/give%20up\\?key="
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
      (should (string-match-p "/json/hapy\\?key="
                              (nth 0 (plist-get result :urls))))
      (should (equal '("No Merriam-Webster entry for \"hapy\", showing \"happy\"")
                     (plist-get result :messages))))))

(ert-deftest mw-thesaurus-lookup-shows-key-error-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) "Invalid API key. Not subscribed for this reference.")
                 (lambda () (mw-thesaurus--lookup-word "happy")))))
    (should-not (plist-get result :rendered))
    (should (equal '("Merriam-Webster: Invalid API key. Not subscribed for this reference.")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-reports-request-failure-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) '(error . "exited abnormally with code 6"))
                 (lambda () (mw-thesaurus--lookup-word "happy")))))
    (should-not (plist-get result :rendered))
    (should (equal '("Merriam-Webster request failed: (error . \"exited abnormally with code 6\")")
                   (plist-get result :messages)))))

(provide 'mw-thesaurus-test)
;;; mw-thesaurus-test.el ends here
