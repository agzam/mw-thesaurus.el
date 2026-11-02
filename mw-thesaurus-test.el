;;; mw-thesaurus-test.el --- Tests for mw-thesaurus -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'mw-thesaurus)

(defun mw-thesaurus-test--asset (file)
  "Return the text of FILE in ./assets, decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (expand-file-name file "assets")))
    (buffer-string)))

(defun mw-thesaurus-test--insert-response (body)
  "Insert an HTTP response with BODY the way `url-retrieve' leaves it."
  (set-buffer-multibyte nil)
  (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n"
          (encode-coding-string body 'utf-8)))

(defun mw-thesaurus-test--read (body)
  "Parse BODY, sent as an HTTP response, with `mw-thesaurus--read-response'."
  (with-temp-buffer
    (mw-thesaurus-test--insert-response body)
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
error for the callback status.  Return a plist of the request URLs, the
texts sent to the thesaurus buffer and the echo area messages."
  (let (urls rendered messages)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (push url urls)
                 (string-match "/json/\\([^?]*\\)" url)
                 (let ((body (funcall respond
                                      (url-unhex-string (match-string 1 url)))))
                   (with-temp-buffer
                     (when (stringp body)
                       (mw-thesaurus-test--insert-response body))
                     (funcall callback (unless (stringp body) (list :error body)))))))
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
                 (lambda (_) '(error http 503))
                 (lambda () (mw-thesaurus--lookup-word "happy")))))
    (should-not (plist-get result :rendered))
    (should (equal '("Merriam-Webster request failed: (error http 503)")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus--create-buffer-reuses-the-thesaurus-window-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        (shown (window-buffer))
        popped)
    (unwind-protect
        (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buffer &rest _) (push buffer popped))))
          (mw-thesaurus--create-buffer "* first ~noun~\n")
          (should (equal 1 (length popped)))
          (set-window-buffer nil mw-thesaurus-buffer-name)
          ;; A `url-retrieve' callback runs in a buffer of its own.
          (with-temp-buffer
            (mw-thesaurus--create-buffer "* second ~noun~\n"))
          (should (equal 1 (length popped)))
          (should (equal "* second ~noun~\n"
                         (with-current-buffer mw-thesaurus-buffer-name
                           (buffer-string)))))
      (set-window-buffer nil shown)
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-package-requires-test ()
  (with-temp-buffer
    (insert-file-contents "mw-thesaurus.el")
    (should (equal '(emacs)
                   (mapcar #'car (package-desc-reqs (package-buffer-info)))))))

(provide 'mw-thesaurus-test)
;;; mw-thesaurus-test.el ends here
