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

(defun mw-thesaurus-test--entries (&rest entries)
  "Return the body of a response with ENTRIES, each (HEADWORD . STEMS)."
  (concat
   "["
   (mapconcat
    (lambda (entry)
      (format "{\"meta\":{\"id\":%s,\"stems\":%s},\"hwi\":{\"hw\":%s},\"fl\":\"verb\",
\"def\":[{\"sseq\":[[[\"sense\",{\"dt\":[[\"text\",\"meaning\"]]}]]]}]}"
              (json-encode (car entry)) (json-encode (vconcat (cdr entry)))
              (json-encode (car entry))))
    entries ",")
   "]"))

(defun mw-thesaurus-test--response (word words suggestions)
  "Return the body the thesaurus API sends for WORD.
Only WORDS have entries.  Any other word gets SUGGESTIONS."
  (if (member word words)
      (mw-thesaurus-test--entries (list word word))
    (json-encode (vconcat suggestions))))

(defun mw-thesaurus-test--call (respond lookup-fn)
  "Call LOOKUP-FN against a fake thesaurus API.
RESPOND takes the requested word and returns the response body, or an
error for the callback status.  Return a plist of the request URLs, the
words and texts sent to the thesaurus buffer and the echo area messages."
  (let (urls words rendered messages)
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
               (lambda (word dict-str) (push word words) (push dict-str rendered)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (funcall lookup-fn))
    (list :urls (nreverse urls)
          :words (nreverse words)
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
    (should (equal '("happy") (plist-get result :words)))
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

(ert-deftest mw-thesaurus-lookup-ignores-suggestions-for-one-letter-test ()
  (let ((result (mw-thesaurus-test--lookup-region "I" '("gi") '("gi" "GI" "is"))))
    (should (equal 1 (length (plist-get result :urls))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"I\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-accepts-two-edits-in-four-letters-test ()
  (let ((result (mw-thesaurus-test--lookup-region "wrod" '("word") '("word" "rod"))))
    (should (equal 2 (length (plist-get result :urls))))
    (should (string-prefix-p "* word ~verb~" (car (plist-get result :rendered))))))

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

(ert-deftest mw-thesaurus-lookup-ignores-entries-of-phrases-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_)
                   (mw-thesaurus-test--entries
                    '("burn" "burn" "burn the candle at both ends")
                    '("cut" "cut" "cut both ways")))
                 (lambda () (mw-thesaurus--lookup-word "Both")))))
    (should (equal 1 (length (plist-get result :urls))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"Both\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-keeps-compounds-of-the-word-test ()
  (let* ((result (mw-thesaurus-test--call
                  (lambda (_)
                    (mw-thesaurus-test--entries
                     '("happy-go-lucky" "happy-go-lucky")
                     '("happy" "happy" "happier" "happiest")))
                  (lambda () (mw-thesaurus--lookup-word "happy"))))
         (org (car (plist-get result :rendered))))
    (should (string-match-p "^\\* happy-go-lucky ~verb~$" org))
    (should (string-match-p "^\\* happy ~verb~$" org))
    (should-not (plist-get result :messages))))

(ert-deftest mw-thesaurus-lookup-drops-entries-of-phrases-beside-the-word-test ()
  (let* ((result (mw-thesaurus-test--call
                  (lambda (_)
                    (mw-thesaurus-test--entries
                     '("neither" "neither")
                     '("neither here nor there" "neither here nor there")
                     '("fish" "fish" "neither fish nor fowl")))
                  (lambda () (mw-thesaurus--lookup-word "neither"))))
         (org (car (plist-get result :rendered))))
    (should (string-match-p "^\\* neither ~verb~$" org))
    (should (string-match-p "^\\* neither here nor there ~verb~$" org))
    (should-not (string-match-p "fish" org))))

(ert-deftest mw-thesaurus-lookup-matches-whole-words-in-headwords-test ()
  (let* ((result (mw-thesaurus-test--call
                  (lambda (_)
                    (mw-thesaurus-test--entries '("is" "is")
                                                '("is friends with" "is friends with")
                                                '("this" "this" "this is it")))
                  (lambda () (mw-thesaurus--lookup-word "is"))))
         (org (car (plist-get result :rendered))))
    (should (string-match-p "^\\* is friends with ~verb~$" org))
    (should-not (string-match-p "this" org))))

(ert-deftest mw-thesaurus-lookup-needs-an-entry-for-the-word-itself-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_)
                   (mw-thesaurus-test--entries '("à la mode" "à la mode")
                                               '("A-list" "A-list")))
                 (lambda () (mw-thesaurus--lookup-word "a")))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster entry for \"a\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-matches-inflections-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_)
                   (mw-thesaurus-test--entries '("weak" "weak" "weaker" "weakest")))
                 (lambda () (mw-thesaurus--lookup-word "Weakest")))))
    (should (string-prefix-p "* weak ~verb~" (car (plist-get result :rendered))))
    (should-not (plist-get result :messages))))

(ert-deftest mw-thesaurus--normalize-test ()
  (should (equal "happy" (mw-thesaurus--normalize "  Happy!  ")))
  (should (equal "well known" (mw-thesaurus--normalize "well-known")))
  (should (equal "under the weather" (mw-thesaurus--normalize "under  the\nweather")))
  (should (equal "naive" (mw-thesaurus--normalize "naïve")))
  (should (equal (mw-thesaurus--normalize "can't") (mw-thesaurus--normalize "can’t"))))

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
          (mw-thesaurus--create-buffer "first" "* first ~noun~\n")
          (should (equal 1 (length popped)))
          (set-window-buffer nil mw-thesaurus-buffer-name)
          ;; A `url-retrieve' callback runs in a buffer of its own.
          (with-temp-buffer
            (mw-thesaurus--create-buffer "second" "* second ~noun~\n"))
          (should (equal 1 (length popped)))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (equal "* second ~noun~\n" (buffer-string)))
            (should (eq 'mw-thesaurus-mode major-mode))))
      (set-window-buffer nil shown)
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-lookup-folds-accents-test ()
  (let ((result (mw-thesaurus-test--lookup-region "café" '("cafe") '("café" "cafe"))))
    (should (equal 1 (length (plist-get result :urls))))
    (should (string-match-p "/json/cafe\\?key=" (car (plist-get result :urls))))
    (should (string-prefix-p "* cafe ~verb~" (car (plist-get result :rendered))))
    (should-not (plist-get result :messages))))

(ert-deftest mw-thesaurus-lookup-shows-the-first-line-of-a-text-reply-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) "<?xml version=\"1.0\"?>\n<entry_list/>")
                 (lambda () (mw-thesaurus--lookup-word "happy")))))
    (should (equal '("Merriam-Webster: <?xml version=\"1.0\"?>")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus--create-buffer-keeps-an-open-prompt-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        displayed popped)
    (unwind-protect
        (cl-letf (((symbol-function 'active-minibuffer-window) #'minibuffer-window)
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (push buffer displayed) nil))
                  ((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buffer &rest _) (push buffer popped))))
          (mw-thesaurus--create-buffer "happy" "* happy ~adjective~\n")
          (should (equal 1 (length displayed)))
          (should-not popped))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-mode-ret-looks-up-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq #'mw-thesaurus-lookup-at-point (key-binding (kbd "RET"))))))

(ert-deftest mw-thesaurus-mode-evil-keys-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq #'mw-thesaurus-lookup-at-point (command-remapping 'evil-ret)))
    (should (eq #'mw-thesaurus--quit (command-remapping 'evil-record-macro)))
    (should (eq #'mw-thesaurus-back (command-remapping 'evil-window-top)))))

(ert-deftest mw-thesaurus-mode-l-goes-back-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq #'mw-thesaurus-back (key-binding (kbd "l"))))))

(defun mw-thesaurus-test--pages (&rest pages)
  "Show PAGES, each (WORD TEXT), in a thesaurus buffer that no window shows."
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore))
    (dolist (page pages)
      ;; `mw-thesaurus--create-buffer' makes its buffer current.
      (with-temp-buffer
        (apply #'mw-thesaurus--create-buffer page)))))

(ert-deftest mw-thesaurus-back-shows-earlier-pages-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        requests)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest args) (push args requests))))
          (mw-thesaurus-test--pages '("happy" "* happy ~adjective~\n")
                                    '("glad" "* glad ~adjective~\n")
                                    '("joyful" "* joyful ~adjective~\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (mw-thesaurus-back)
            (should (equal "* glad ~adjective~\n" (buffer-string)))
            (should (equal "glad" mw-thesaurus--word))
            (should (eq 'mw-thesaurus-mode major-mode))
            (should buffer-read-only)
            (mw-thesaurus-back)
            (should (equal "* happy ~adjective~\n" (buffer-string)))
            (should-error (mw-thesaurus-back) :type 'user-error)
            (should (equal "* happy ~adjective~\n" (buffer-string))))
          (should-not requests))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-back-restores-point-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*"))
    (unwind-protect
        (progn
          (mw-thesaurus-test--pages
           '("happy" "* happy ~adjective~\n*** Synonyms:\n    glad, joyful\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (goto-char (point-min))
            (search-forward "joyful")
            (goto-char (match-beginning 0)))
          (mw-thesaurus-test--pages '("joyful" "* joyful ~adjective~\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (bobp))
            (mw-thesaurus-back)
            (should (looking-at-p "joyful"))))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-back-forgets-a-killed-buffer-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*"))
    (unwind-protect
        (progn
          (mw-thesaurus-test--pages '("happy" "* happy ~adjective~\n")
                                    '("glad" "* glad ~adjective~\n"))
          (kill-buffer mw-thesaurus-buffer-name)
          (mw-thesaurus-test--pages '("sad" "* sad ~adjective~\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (should-error (mw-thesaurus-back) :type 'user-error)))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-mode-derives-from-org-mode-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq 'mw-thesaurus-mode major-mode))
    (should (derived-mode-p 'org-mode))
    (should (eq org-mode-map (keymap-parent mw-thesaurus-mode-map)))
    (should buffer-read-only)
    (should (local-variable-p 'org-hide-emphasis-markers))
    (should org-hide-emphasis-markers)
    (should-not (default-value 'org-hide-emphasis-markers))))

(ert-deftest mw-thesaurus-mode-map-overrides-org-keys-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq #'org-ctrl-c-ctrl-c (key-binding (kbd "C-c C-c"))))
    (unwind-protect
        (progn
          (define-key mw-thesaurus-mode-map (kbd "C-c C-c") #'ignore)
          (should (eq #'ignore (key-binding (kbd "C-c C-c")))))
      (define-key mw-thesaurus-mode-map (kbd "C-c C-c") nil))))

(ert-deftest mw-thesaurus-reload-resets-the-api-url-test ()
  (let ((url mw-thesaurus--api-url))
    (unwind-protect
        (progn
          (setq mw-thesaurus--api-url
                "http://www.dictionaryapi.com/api/v1/references/thesaurus/xml/")
          (load (expand-file-name "mw-thesaurus.el") nil t)
          (should (equal url mw-thesaurus--api-url)))
      (setq mw-thesaurus--api-url url))))

(ert-deftest mw-thesaurus-package-requires-test ()
  (with-temp-buffer
    (insert-file-contents "mw-thesaurus.el")
    (should (equal '(emacs)
                   (mapcar #'car (package-desc-reqs (package-buffer-info)))))))

(provide 'mw-thesaurus-test)
;;; mw-thesaurus-test.el ends here
