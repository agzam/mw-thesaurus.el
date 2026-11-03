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

(defconst mw-thesaurus-test--dictionary-words
  '("decline" "set" "conception" "cowl" "happy" "both" "tearaway" "borstal" "went")
  "Words with a Collegiate Dictionary response in ./assets.")

(defun mw-thesaurus-test--dictionary (word)
  "Return the Org text of the dictionary response for WORD in ./assets.
The zero-width spaces around italics are left out."
  (replace-regexp-in-string
   "\u200b" ""
   (mw-thesaurus--dictionary-parse
    (mw-thesaurus-test--read
     (mw-thesaurus-test--asset (format "dictionary-%s.json" word))))))

(ert-deftest mw-thesaurus--dictionary-parse-test ()
  (should (equal (mw-thesaurus-test--asset "dictionary-decline.org")
                 (mw-thesaurus-test--dictionary "decline"))))

(ert-deftest mw-thesaurus--dictionary-parse-leaves-no-markup-test ()
  (dolist (word mw-thesaurus-test--dictionary-words)
    (let ((org (mw-thesaurus-test--dictionary word)))
      (should (string-prefix-p "* " org))
      (should-not (string-match-p "[{}]" org))
      (should-not (string-match-p "^\\*+ *$" org))
      ;; Org shows a line that starts with a colon as fixed-width text.
      (should-not (string-match-p "^ *:" org)))))

(ert-deftest mw-thesaurus--dictionary-parse-sense-groups-test ()
  (let ((org (mw-thesaurus-test--dictionary "conception")))
    (should (string-match-p
             (concat "^  1\\.\n"
                     "     a\\.\n"
                     "        1) the process of becoming pregnant involving"
                     " fertilization or implantation or both\n"
                     "        2) embryo, fetus\n"
                     "     b\\. beginning\n"
                     "        \"Joy had the like /conception/ in our eyes …\""
                     " — William Shakespeare\n"
                     "  2\\.\n"
                     "     a\\. the capacity, ")
             org))
    (should (string-match-p
             (concat "^   Etymology: Middle English /concepcioun/, from Anglo-French"
                     " /concepcion/, from Latin /conception-, conceptio/, from"
                     " /concipere/ — see concept entry 1\n"
                     "   First known use: 14th century\n")
             org))))

(ert-deftest mw-thesaurus--dictionary-parse-senses-without-numbers-test ()
  (should (string-match-p
           (concat "^\\* cowl ~verb~\n  cowled; cowling; cowls\n"
                   "\\*\\* transitive verb\n   to cover with or as if with a cowl\n"
                   "\\*\\* Word history\n   First known use: 1536\n")
           (mw-thesaurus-test--dictionary "cowl")))
  (should (string-match-p "^     c\\. cowling$" (mw-thesaurus-test--dictionary "cowl")))
  (should (equal (concat "* tearaway ~noun~\n"
                         "  tear·away [[mw-thesaurus-audio:tearaw01][ˈter-ə-ˌwā]]\n"
                         "  /British/\n"
                         "  a rebellious and unruly or reckless young person\n"
                         "** Word history\n   First known use: 1950\n"
                         "* tear away ~verb~\n"
                         "  tore away; torn away; tearing away; tears away\n"
                         "** transitive verb\n"
                         "   to remove (someone, such as oneself) reluctantly\n"
                         "** Word history\n   First known use: circa 1699\n")
                 (mw-thesaurus-test--dictionary "tearaway")))
  (should (equal (concat "* Borstal ~noun~\n"
                         "  Bor·stal [[mw-thesaurus-audio:borsta01][ˈbȯr-stᵊl]]\n"
                         "  /British/\n"
                         "  reformatory\n"
                         "** Word history\n"
                         "   Etymology: /Borstal/, English village where the first"
                         " such institution was set up\n"
                         "   First known use: 1907\n")
                 (mw-thesaurus-test--dictionary "borstal"))))

(ert-deftest mw-thesaurus--dictionary-parse-biographical-names-test ()
  (should (string-match-p
           (concat "^\\* Cowl ~biographical name~\n"
                   "  \\[\\[mw-thesaurus-audio:bixcow04\\]\\[ˈkau̇(-ə)l\\]\\]\n"
                   "  Jane 1883–1950 originally /Grace Bailey/ American actress\n")
           (mw-thesaurus-test--dictionary "cowl"))))

(ert-deftest mw-thesaurus--dictionary-parse-divided-senses-test ()
  (let ((happy (mw-thesaurus-test--dictionary "happy")))
    (should (string-match-p
             (concat "^  1\\. informal talk among the participants in a television"
                     " news broadcast\n"
                     "     /also/ : a broadcast format featuring such talk\n"
                     "  2\\. optimistic talk\n")
             happy))
    (should (string-match-p
             (concat "^  1\\. irresponsible in the use of firearms\n"
                     "     /especially/ : inclined to shoot before clearly identifying"
                     " the target\n")
             happy)))
  (should (string-match-p
           (concat "^   10\\. to fix or decide on as a time, limit, or regulation : prescribe\n"
                   "       /set/ a wedding day\n")
           (mw-thesaurus-test--dictionary "set")))
  (should (string-match-p
           (concat "^   12\\.\n"
                   "      a\\. to adjust (a device and especially a measuring"
                   " device) to a desired position\n"
                   "         /set/ the alarm for 7:00\n"
                   "         /set/ a thermostat at 68\n"
                   "         /also/ : to adjust (something, such as a clock) in conformity"
                   " with a standard\n")
           (mw-thesaurus-test--dictionary "set"))))

(ert-deftest mw-thesaurus--dictionary-parse-usage-notes-test ()
  (let ((org (mw-thesaurus-test--dictionary "both")))
    (should (string-match-p
             (concat "^\\* both ~conjunction~\n"
                     "  used as a function word to indicate and stress the"
                     " inclusion of each of two or more things specified by"
                     " coordinated words, phrases, or clauses\n"
                     "  prized /both/ for its beauty and for its utility\n"
                     "  he … who loveth well /both/ man and bird and beast"
                     " — S. T. Coleridge\n"
                     "\\*\\* Word history\n"
                     "   Etymology: Middle English /bothe,/ from the use of the"
                     " quantifier /bothe/ both as a correlative conjunction\n")
             org))
    (should (string-match-p
             "^   Note: Middle English /bothe/ is also thought to reflect the Old Norse"
             org))))

(ert-deftest mw-thesaurus--dictionary-parse-cognate-entries-test ()
  (should (equal (concat "* went\n  past tense of go\n"
                         "** Word history\n"
                         "   Etymology: Middle English, past & past participle of"
                         " /wenden/ to wend\n")
                 (mw-thesaurus-test--dictionary "went"))))

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

(defun mw-thesaurus-test--fake-api (respond)
  "Return a fake `url-retrieve' that answers with RESPOND.
RESPOND takes the requested word and returns the response body, or an
error for the callback status."
  (lambda (url callback &rest _)
    (string-match "/json/\\([^?]*\\)" url)
    (let ((body (funcall respond (url-unhex-string (match-string 1 url)))))
      (with-temp-buffer
        (when (stringp body)
          (mw-thesaurus-test--insert-response body))
        (funcall callback (unless (stringp body) (list :error body)))))))

(defun mw-thesaurus-test--call (respond lookup-fn)
  "Call LOOKUP-FN against a fake Merriam-Webster API.
RESPOND takes the requested word and returns the response body, or an
error for the callback status.  Return a plist of the request URLs, the
words, texts and references sent to the buffer and the echo area messages."
  (let ((api (mw-thesaurus-test--fake-api respond))
        urls words rendered references messages)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest args) (push url urls) (apply api url args)))
              ((symbol-function 'mw-thesaurus--create-buffer)
               (lambda (word dict-str &optional reference)
                 (push word words) (push dict-str rendered) (push reference references)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (funcall lookup-fn))
    (list :urls (nreverse urls)
          :words (nreverse words)
          :rendered (nreverse rendered)
          :references (nreverse references)
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
    (should (equal '("Merriam-Webster: Invalid API key. Not subscribed for this reference.  Set `mw-thesaurus-api-key' to a Collegiate Thesaurus key from https://dictionaryapi.com")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-reports-request-failure-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) '(error http 503))
                 (lambda () (mw-thesaurus--lookup-word "happy")))))
    (should-not (plist-get result :rendered))
    (should (equal '("Merriam-Webster request failed: (error http 503)")
                   (plist-get result :messages)))))

(defun mw-thesaurus-test--dictionary-response (word)
  "Return the dictionary response for WORD: decline, or suggestions."
  (if (equal word "decline")
      (mw-thesaurus-test--asset "dictionary-decline.json")
    "[\"decline\",\"decliner\"]"))

(ert-deftest mw-thesaurus-dictionary-lookup-dwim-test ()
  (with-temp-buffer
    (insert "in decline")
    (goto-char 5)
    (let ((result (mw-thesaurus-test--call #'mw-thesaurus-test--dictionary-response
                                           #'mw-thesaurus-dictionary-lookup-dwim)))
      (should (equal (list (concat mw-thesaurus--dictionary-api-url "decline?key="
                                   mw-thesaurus-dictionary-api-key))
                     (plist-get result :urls)))
      (should (equal '("decline") (plist-get result :words)))
      (should (equal '(dictionary) (plist-get result :references)))
      (should (string-prefix-p "* decline ~verb~\n  de·cline [[mw-thesaurus-audio:declin03]"
                               (car (plist-get result :rendered))))
      (should-not (plist-get result :messages)))))

(ert-deftest mw-thesaurus-dictionary-lookup-falls-back-test ()
  (let ((result (mw-thesaurus-test--call
                 #'mw-thesaurus-test--dictionary-response
                 (lambda () (mw-thesaurus--lookup-word "declne" 'dictionary)))))
    (should (equal 2 (length (plist-get result :urls))))
    (should (string-match-p "/collegiate/json/decline\\?" (nth 1 (plist-get result :urls))))
    (should (equal '(dictionary) (plist-get result :references)))
    (should (equal '("No Merriam-Webster dictionary entry for \"declne\", showing \"decline\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-dictionary-lookup-reports-a-miss-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) "[]")
                 (lambda () (mw-thesaurus--lookup-word "xqzvbnwpt" 'dictionary)))))
    (should-not (plist-get result :rendered))
    (should (equal '("No Merriam-Webster dictionary entry for \"xqzvbnwpt\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-dictionary-lookup-without-a-key-test ()
  (let* ((mw-thesaurus-dictionary-api-key nil)
         (result (mw-thesaurus-test--call
                  #'mw-thesaurus-test--dictionary-response
                  (lambda ()
                    (should (equal (list 'user-error "Set `mw-thesaurus-dictionary-api-key' to a Collegiate Dictionary key from https://dictionaryapi.com")
                                   (should-error (mw-thesaurus--lookup-word "decline" 'dictionary)
                                                 :type 'user-error)))))))
    (should-not (plist-get result :urls))))

(ert-deftest mw-thesaurus-dictionary-lookup-shows-key-error-test ()
  (let ((result (mw-thesaurus-test--call
                 (lambda (_) "Invalid API key. Not subscribed for this reference.")
                 (lambda () (mw-thesaurus--lookup-word "decline" 'dictionary)))))
    (should-not (plist-get result :rendered))
    (should (equal '("Merriam-Webster dictionary: Invalid API key. Not subscribed for this reference.  Set `mw-thesaurus-dictionary-api-key' to a Collegiate Dictionary key from https://dictionaryapi.com")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus-lookup-at-point-follows-the-page-test ()
  (with-temp-buffer
    (insert "to slope downward")
    (setq-local mw-thesaurus--reference 'dictionary)
    (goto-char 5)
    (let ((result (mw-thesaurus-test--call
                   (lambda (_) "[]")
                   (lambda ()
                     (call-interactively #'mw-thesaurus-lookup-at-point)
                     (mw-thesaurus-lookup-dwim)))))
      (should (string-match-p "/collegiate/json/slope\\?" (nth 0 (plist-get result :urls))))
      (should (string-match-p "/thesaurus/json/slope\\?" (nth 1 (plist-get result :urls)))))))

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

(ert-deftest mw-thesaurus-mode-writable-page-types-keys-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (read-only-mode -1)
    (should (eq #'org-self-insert-command (key-binding "q")))
    (should (eq #'org-self-insert-command (key-binding "l")))
    (should (eq #'org-return (key-binding (kbd "RET"))))
    (should (eq #'org-self-insert-command (key-binding "o")))
    (should-not (command-remapping 'evil-ret))
    (should-not (command-remapping 'evil-record-macro))
    (should-not (command-remapping 'evil-window-top))
    (should-not (command-remapping 'evil-open-below))
    (should (eq #'mw-thesaurus-lookup-at-point (key-binding (kbd "C-c C-o"))))
    (read-only-mode 1)
    (should (eq #'mw-thesaurus--quit (key-binding "q")))
    (should (eq #'mw-thesaurus-back (command-remapping 'evil-window-top)))
    (should (eq #'mw-thesaurus-lookup-at-point (key-binding (kbd "C-c C-o"))))))

(ert-deftest mw-thesaurus-mode-o-switches-reference-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    (should (eq #'mw-thesaurus-switch-reference (key-binding "o")))
    (should (eq #'mw-thesaurus-switch-reference (command-remapping 'evil-open-below)))
    (should (eq #'mw-thesaurus-switch-reference (command-remapping 'evil-org-open-below)))))

(ert-deftest mw-thesaurus-header-line-marks-the-reference-test ()
  (with-temp-buffer
    (mw-thesaurus-mode)
    ;; `format-mode-line' gives an empty string in batch mode.
    (should (equal '(:eval (mw-thesaurus--header-line)) header-line-format))
    (setq mw-thesaurus--reference 'dictionary)
    (let ((line (mw-thesaurus--header-line)))
      (should (equal " Thesaurus   Dictionary" (substring-no-properties line)))
      (should (eq 'bold (get-text-property (string-match "Dictionary" line) 'face line)))
      (should-not (get-text-property (string-match "Dictionary" line) 'local-map line))
      (let ((other (string-match "Thesaurus" line)))
        (should (eq 'shadow (get-text-property other 'face line)))
        (should (eq #'mw-thesaurus-switch-reference
                    (lookup-key (get-text-property other 'local-map line)
                                [header-line mouse-1])))))))

(ert-deftest mw-thesaurus-mode-keeps-org-overriding-maps-test ()
  (let ((org-modes (with-temp-buffer
                     (org-mode)
                     (mapcar #'car minor-mode-overriding-map-alist))))
    (with-temp-buffer
      (mw-thesaurus-mode)
      (should (equal (cons 'buffer-read-only org-modes)
                     (mapcar #'car minor-mode-overriding-map-alist))))))

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

(ert-deftest mw-thesaurus-switch-reference-and-back-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        (dictionary (mw-thesaurus-test--fake-api
                     (lambda (_) (mw-thesaurus-test--asset "dictionary-decline.json"))))
        (thesaurus (mw-thesaurus-test--fake-api
                    (lambda (_) (mw-thesaurus-test--asset "sample.json"))))
        urls)
    (unwind-protect
        (progn
          (mw-thesaurus-test--pages '("decline" "* decline ~verb~\n"))
          (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
                    ((symbol-function 'url-retrieve)
                     (lambda (url &rest args)
                       (push url urls)
                       (apply (if (string-match-p "/collegiate/" url) dictionary thesaurus)
                              url args))))
            ;; The `url-retrieve' callback runs in a buffer of its own.
            (with-temp-buffer (mw-thesaurus-switch-reference))
            (with-temp-buffer (mw-thesaurus-switch-reference)))
          (should (string-match-p "/collegiate/json/decline\\?" (nth 1 urls)))
          (should (string-match-p "/thesaurus/json/decline\\?" (nth 0 urls)))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (eq 'thesaurus mw-thesaurus--reference))
            (should (equal (mw-thesaurus-test--asset "sample.org") (buffer-string)))
            (mw-thesaurus-back)
            (should (eq 'dictionary mw-thesaurus--reference))
            (should (string-prefix-p "* decline ~verb~\n  de·cline" (buffer-string)))
            (mw-thesaurus-back)
            (should (eq 'thesaurus mw-thesaurus--reference))
            (should (equal "* decline ~verb~\n" (buffer-string)))))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-switch-reference-from-the-dictionary-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*"))
    (unwind-protect
        (progn
          (mw-thesaurus-test--pages '("decline" "* decline ~verb~\n" dictionary))
          (let ((result (mw-thesaurus-test--call (lambda (_) "[]")
                                                 #'mw-thesaurus-switch-reference)))
            (should (string-match-p "/thesaurus/json/decline\\?"
                                    (car (plist-get result :urls))))
            ;; A miss keeps the page.
            (should (equal '("No Merriam-Webster entry for \"decline\"")
                           (plist-get result :messages))))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (eq 'dictionary mw-thesaurus--reference))))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus-switch-reference-needs-a-page-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test-none*"))
    (should-error (mw-thesaurus-switch-reference) :type 'user-error)))

(ert-deftest mw-thesaurus--audio-url-test ()
  (dolist (case '(("declin03" . "d/declin03") ("bixcow04" . "bix/bixcow04")
                  ("ggmain01" . "gg/ggmain01") ("3d000001" . "number/3d000001")
                  ("_abc001" . "number/_abc001")))
    (should (equal (concat "https://media.merriam-webster.com/audio/prons/en/us/mp3/"
                           (cdr case) ".mp3")
                   (mw-thesaurus--audio-url (car case))))))

(ert-deftest mw-thesaurus--dictionary-parse-pronunciation-links-test ()
  (let ((org (mw-thesaurus-test--dictionary "decline")))
    (should (string-match-p
             "^  de·cline \\[\\[mw-thesaurus-audio:declin03\\]\\[di-ˈklīn\\]\\] dē-\n  declined; declining$"
             org))
    (should (string-match-p
             (concat "^  de·cline \\[\\[mw-thesaurus-audio:declin03\\]\\[di-ˈklīn\\]\\] dē-"
                     " also \\[\\[mw-thesaurus-audio:declin06\\]\\[ˈdē-ˌklīn\\]\\]$")
             org))))

(ert-deftest mw-thesaurus--dictionary-parse-marks-faces-test ()
  (let* ((org (mw-thesaurus--dictionary-parse
               (mw-thesaurus-test--read (mw-thesaurus-test--asset "dictionary-decline.json"))))
         (face-at (lambda (text)
                    (get-text-property (string-match (regexp-quote text) org)
                                       'mw-thesaurus-face org))))
    (should (eq 'mw-thesaurus-syllables (funcall face-at "de·cline")))
    (should (eq 'mw-thesaurus-pronunciation (funcall face-at "dē-")))
    (should (eq 'mw-thesaurus-pronunciation (funcall face-at "also")))
    (should (eq 'mw-thesaurus-example (funcall face-at "The price of the stock")))
    (should (eq 'mw-thesaurus-example (funcall face-at "— Charles Dickens")))
    (should-not (funcall face-at "[[mw-thesaurus-audio:declin03]"))
    (should-not (funcall face-at "declined; declining"))
    (should-not (funcall face-at "to become lower"))))

(ert-deftest mw-thesaurus--sense-bullets-test ()
  (should (equal '((0 . "1.") (1 . "a.") (2 . "1)")) (mw-thesaurus--sense-bullets "1 a (1)")))
  (should (equal '((1 . "c.") (2 . "1)")) (mw-thesaurus--sense-bullets "c (1)")))
  (should (equal '((1 . "b.")) (mw-thesaurus--sense-bullets "b")))
  (should (equal '((2 . "2)")) (mw-thesaurus--sense-bullets "(2)")))
  (should (equal '((0 . "12.")) (mw-thesaurus--sense-bullets "12")))
  (should-not (mw-thesaurus--sense-bullets nil)))

(defun mw-thesaurus-test--decline-page ()
  "Return the dictionary page for \"decline\" in ./assets, as the parser makes it."
  (mw-thesaurus--dictionary-parse
   (mw-thesaurus-test--read (mw-thesaurus-test--asset "dictionary-decline.json"))))

(ert-deftest mw-thesaurus--show-gives-marked-text-its-face-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        (face-at (lambda (text)
                   (goto-char (point-min))
                   (search-forward text)
                   (get-char-property (match-beginning 0) 'face))))
    (unwind-protect
        (progn
          (mw-thesaurus-test--pages (list "decline" (mw-thesaurus-test--decline-page) 'dictionary))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (eq 'mw-thesaurus-syllables (funcall face-at "de·cline")))
            (should (eq 'mw-thesaurus-example (funcall face-at "Employee morale"))))
          (mw-thesaurus-test--pages '("sun" "* sun ~noun~\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (should-not (overlays-in (point-min) (point-max)))
            (mw-thesaurus-back)
            (should (eq 'mw-thesaurus-pronunciation (funcall face-at "dē-")))
            (should (eq 'mw-thesaurus-example (funcall face-at "Employee morale")))
            (should-not (funcall face-at "to become lower"))))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus--play-downloads-once-test ()
  (let* ((temporary-file-directory (make-temp-file "mw-audio" t))
         (mw-thesaurus-audio-player '("player" "-q"))
         downloads commands)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url callback &rest _)
                     (push url downloads)
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert "HTTP/1.1 200 OK\r\nContent-Type: audio/mpeg\r\n\r\nID3\377\373")
                       (funcall callback nil))))
                  ((symbol-function 'make-process)
                   (lambda (&rest args) (push (plist-get args :command) commands))))
          (mw-thesaurus--play "declin03")
          (mw-thesaurus--play "declin03")
          (let ((file (expand-file-name "mw-thesaurus/declin03.mp3" temporary-file-directory)))
            (should (equal '("https://media.merriam-webster.com/audio/prons/en/us/mp3/d/declin03.mp3")
                           downloads))
            (should (equal (list (list "player" "-q" file) (list "player" "-q" file)) commands))
            (should (equal "ID3\377\373"
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))))))
      (delete-directory temporary-file-directory t))))

(ert-deftest mw-thesaurus--play-without-a-player-test ()
  (let ((mw-thesaurus-audio-player nil))
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (should-error (mw-thesaurus--play "declin03") :type 'user-error))))

(ert-deftest mw-thesaurus-ret-plays-a-pronunciation-test ()
  (should (eq #'mw-thesaurus--play (org-link-get-parameter "mw-thesaurus-audio" :follow)))
  (let (played)
    (cl-letf (((symbol-function 'mw-thesaurus--play) (lambda (audio &rest _) (push audio played)))
              ((symbol-function 'url-retrieve) (lambda (&rest _) (error "No lookup expected"))))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (insert "* decline ~verb~\n  de·cline [[mw-thesaurus-audio:declin03][di-ˈklīn]] dē-\n"))
        (mw-thesaurus-mode)
        (goto-char (point-min))
        (search-forward "klīn")
        (call-interactively #'mw-thesaurus-lookup-at-point)))
    (should (equal '("declin03") played))))

(defun mw-thesaurus-test--gui-color-values (color &optional _frame)
  "Return the RGB values of COLOR as a graphic frame does.
A batch Emacs gives the nearest of its few terminal colors."
  (tty-color-standard-values color))

(ert-deftest mw-thesaurus--svg-color-test ()
  (cl-letf (((symbol-function 'color-values) #'mw-thesaurus-test--gui-color-values))
    (should (equal "#343a47" (mw-thesaurus--svg-color "#33cd39d34713")))
    (should (equal "#8fa1b3" (mw-thesaurus--svg-color "#8fa1b3")))
    (should (equal "#ffffff" (mw-thesaurus--svg-color "white")))
    (should (equal "#616d7d" (mw-thesaurus--svg-color "#33cd39d34713" "#8fa1b3" 0.5)))))

(defconst mw-thesaurus-test--font
  ["Fira Code-18" "Fira Code" 18 23 0 0 0 11 17 6 11 11 nil nil]
  "The `font-info' of an 18-pixel font: a 23-pixel line, ascent 17, descent 6.")

(defun mw-thesaurus-test--svg (text)
  "Return the button for TEXT drawn for `mw-thesaurus-test--font' and a dark theme."
  (cl-letf (((symbol-function 'color-values) #'mw-thesaurus-test--gui-color-values)
            ((symbol-function 'default-font-width) (lambda () 11))
            ((symbol-function 'face-foreground) (lambda (&rest _) "#8fa1b3"))
            ((symbol-function 'face-background) (lambda (&rest _) "#33cd39d34713")))
    (mw-thesaurus--audio-svg text mw-thesaurus-test--font)))

(defun mw-thesaurus-test--speaker-left (svg)
  "Return the left edge of the speaker in SVG."
  (seq-min (seq-map-indexed (lambda (number index) (if (cl-evenp index) number 1000))
                            (mapcar #'string-to-number
                                    (split-string (dom-attr (car (dom-by-tag svg 'polygon))
                                                            'points)
                                                  "[ ,]+" t)))))

(ert-deftest mw-thesaurus--audio-svg-test ()
  (let* ((svg (mw-thesaurus-test--svg "di-ˈklīn"))
         (rect (car (dom-by-tag svg 'rect)))
         (text (car (dom-by-tag svg 'text))))
    (should (equal 27 (dom-attr svg 'height)))
    (should (equal 133 (dom-attr svg 'width)))
    (should (equal 13.5 (dom-attr rect 'rx)))
    (should (equal "#3f4654" (dom-attr rect 'fill)))
    (should (equal "#616d7d" (dom-attr rect 'stroke)))
    (should (string-prefix-p "di-" (dom-text text)))
    (should (equal "#8fa1b3" (dom-attr text 'fill)))
    (should (equal 13.5 (dom-attr text 'x)))
    ;; Emacs puts the baseline 19 pixels down a 27-pixel image it centers.
    (should (equal 19 (dom-attr text 'y)))
    ;; The speaker and its two waves come after 8 columns of text.
    (should (< (+ 13.5 (* 8 11)) (mw-thesaurus-test--speaker-left svg)))
    (should (equal 2 (length (dom-by-tag svg 'path))))))

(ert-deftest mw-thesaurus--audio-svg-bare-speaker-test ()
  (let ((svg (mw-thesaurus-test--svg "")))
    (should-not (dom-by-tag svg 'text))
    (should (equal 27 (dom-attr svg 'height)))
    (should (equal 35 (dom-attr svg 'width)))
    ;; As far from the left edge as the waves end from the right one.
    (should (< 10.7 (mw-thesaurus-test--speaker-left svg) 10.9))
    (should (equal 2 (length (dom-by-tag svg 'path))))))

(ert-deftest mw-thesaurus--show-draws-pronunciation-buttons-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        played)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                  ((symbol-function 'font-info) (lambda (&rest _) mw-thesaurus-test--font))
                  ((symbol-function 'mw-thesaurus--audio-svg)
                   (lambda (text font) (should (eq mw-thesaurus-test--font font)) (list 'svg nil text)))
                  ((symbol-function 'svg-image)
                   (lambda (svg &rest props) (append (list 'image :svg svg) props)))
                  ((symbol-function 'mw-thesaurus--play)
                   (lambda (audio &rest _) (push audio played))))
          (mw-thesaurus-test--pages (list "decline" (mw-thesaurus-test--decline-page) 'dictionary))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (equal 3 (seq-count (lambda (overlay) (overlay-get overlay 'button))
                                        (overlays-in (point-min) (point-max)))))
            (goto-char (point-min))
            (search-forward "[[mw-thesaurus-audio:declin06][ˈdē-ˌklīn]]")
            (let ((button (button-at (match-beginning 0))))
              (should (equal (match-end 0) (button-end button)))
              (should (equal '(image :svg (svg nil "ˈdē-ˌklīn") :scale 1 :ascent center)
                             (button-get button 'display)))
              (should (equal '(:underline nil) (button-get button 'face)))
              (should (button-get button 'follow-link))
              (button-activate button))
            (should (equal '("declin06") played))))
      (kill-buffer mw-thesaurus-buffer-name))))

(ert-deftest mw-thesaurus--show-keeps-pronunciation-links-without-svg-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*"))
    (unwind-protect
        (dolist (display '((nil . t) (t . nil)))
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) (car display)))
                    ((symbol-function 'image-type-available-p) (lambda (&rest _) (cdr display))))
            (mw-thesaurus-test--pages
             (list "decline" (mw-thesaurus-test--decline-page) 'dictionary))
            (with-current-buffer mw-thesaurus-buffer-name
              (should (search-forward "[[mw-thesaurus-audio:declin03]" nil t))
              (should-not (seq-some (lambda (overlay) (overlay-get overlay 'button))
                                    (overlays-in (point-min) (point-max)))))))
      (kill-buffer mw-thesaurus-buffer-name))))

(defconst mw-thesaurus-test--decline-verb "ee2c507f-4c87-42f1-828f-140e7adc6c39"
  "The uuid of the Collegiate Dictionary entry for the verb decline.")

(ert-deftest mw-thesaurus--show-draws-speakers-on-a-thesaurus-page-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        pressed)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                  ((symbol-function 'font-info) (lambda (&rest _) mw-thesaurus-test--font))
                  ((symbol-function 'mw-thesaurus--audio-svg)
                   (lambda (text font) (should (eq mw-thesaurus-test--font font)) (list 'svg nil text)))
                  ((symbol-function 'svg-image)
                   (lambda (svg &rest props) (append (list 'image :svg svg) props)))
                  ((symbol-function 'mw-thesaurus--entry-audio)
                   (lambda (path &rest _) (push path pressed))))
          (mw-thesaurus-test--pages (list "decline" (mw-thesaurus-test--asset "sample.org")))
          (with-current-buffer mw-thesaurus-buffer-name
            (should (equal 2 (seq-count (lambda (overlay) (overlay-get overlay 'button))
                                        (overlays-in (point-min) (point-max)))))
            (goto-char (point-min))
            (search-forward "[[mw-thesaurus-entry-audio:")
            (let ((button (button-at (match-beginning 0))))
              ;; The speaker alone: the thesaurus has no pronunciation to show.
              (should (equal '(image :svg (svg nil "") :scale 1 :ascent center)
                             (button-get button 'display)))
              (button-activate button))
            (should (equal (list (concat mw-thesaurus-test--decline-verb ":decline")) pressed))))
      (kill-buffer mw-thesaurus-buffer-name))))

(defun mw-thesaurus-test--entry-audio (paths respond)
  "Follow the entry links PATHS, in order, against a fake API answering RESPOND.
Return what `mw-thesaurus-test--call' returns, plus the recordings played."
  (let ((mw-thesaurus--entry-recordings (make-hash-table :test #'equal))
        (mw-thesaurus-audio-player '("true"))
        played)
    (cl-letf (((symbol-function 'mw-thesaurus--play)
               (lambda (audio &rest _) (push audio played))))
      (append (mw-thesaurus-test--call
               respond (lambda () (mapc #'mw-thesaurus--entry-audio paths)))
              (list :played (nreverse played))))))

(ert-deftest mw-thesaurus--entry-audio-asks-the-dictionary-once-test ()
  (let ((result (mw-thesaurus-test--entry-audio
                 (make-list 2 (concat mw-thesaurus-test--decline-verb ":decline"))
                 (lambda (_) (mw-thesaurus-test--asset "dictionary-decline.json")))))
    (should (equal 1 (length (plist-get result :urls))))
    (should (string-match-p "/collegiate/json/decline\\?key=" (car (plist-get result :urls))))
    (should (equal '("declin03" "declin03") (plist-get result :played)))
    (should-not (plist-get result :messages))))

(ert-deftest mw-thesaurus--entry-audio-without-a-recording-test ()
  ;; The phrase "in decline" has no pronunciation; the zero uuid is no entry.
  (let ((result (mw-thesaurus-test--entry-audio
                 '("de9b6681-0be1-46c1-87f7-fd52dc07ce25:in decline"
                   "de9b6681-0be1-46c1-87f7-fd52dc07ce25:in decline"
                   "00000000-0000-0000-0000-000000000000:decline")
                 (lambda (_) (mw-thesaurus-test--asset "dictionary-decline.json")))))
    (should (equal 2 (length (plist-get result :urls))))
    (should (string-match-p "/collegiate/json/in%20decline\\?" (car (plist-get result :urls))))
    (should-not (plist-get result :played))
    (should (equal '("No Merriam-Webster recording of \"in decline\""
                     "No Merriam-Webster recording of \"in decline\""
                     "No Merriam-Webster recording of \"decline\"")
                   (plist-get result :messages)))))

(ert-deftest mw-thesaurus--entry-audio-after-a-failed-request-test ()
  (let ((result (mw-thesaurus-test--entry-audio
                 (make-list 2 (concat mw-thesaurus-test--decline-verb ":decline"))
                 (lambda (_) '(error connection-failed "refused")))))
    ;; A failure is not remembered: the next press asks again.
    (should (equal 2 (length (plist-get result :urls))))
    (should-not (plist-get result :played))
    (should (string-match-p "\\`Merriam-Webster dictionary request failed"
                            (car (plist-get result :messages))))))

(ert-deftest mw-thesaurus--entry-audio-needs-a-player-and-a-key-test ()
  (let ((mw-thesaurus--entry-recordings (make-hash-table :test #'equal))
        (path (concat mw-thesaurus-test--decline-verb ":decline")))
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (error "No request expected"))))
      (let ((mw-thesaurus-audio-player nil))
        (cl-letf (((symbol-function 'executable-find) #'ignore))
          (should-error (mw-thesaurus--entry-audio path) :type 'user-error)))
      (let ((mw-thesaurus-audio-player '("true"))
            (mw-thesaurus-dictionary-api-key nil))
        (should-error (mw-thesaurus--entry-audio path) :type 'user-error)))))

(ert-deftest mw-thesaurus--parse-links-entries-of-the-dictionary-only-test ()
  (let ((org (mw-thesaurus--parse
              (mw-thesaurus-test--read
               (concat "[{\"meta\":{\"id\":\"a\",\"target\":{\"tuuid\":\"u1\",\"tsrc\":\"collegiate\"}},"
                       "\"hwi\":{\"hw\":\"run along\"},\"fl\":\"verb\"},"
                       "{\"meta\":{\"id\":\"b\",\"target\":{\"tuuid\":\"u2\",\"tsrc\":\"learners\"}},"
                       "\"hwi\":{\"hw\":\"run\"},\"fl\":\"noun\"}]")))))
    (should (equal (concat "* run along ~verb~ [[mw-thesaurus-entry-audio:u1:run along][🔊]]\n"
                           "* run ~noun~\n")
                   org))))

(ert-deftest mw-thesaurus--show-leaves-indentation-to-org-indent-mode-test ()
  (let ((mw-thesaurus-buffer-name " *mw-thesaurus-test*")
        (shown (concat "* decline ~verb~\nde·cline\n** intransitive verb\n"
                       "4.\n   a. to sink\n      the declining sun\n")))
    (unwind-protect
        (progn
          (add-hook 'org-mode-hook #'org-indent-mode)
          (mw-thesaurus-test--pages
           (list "decline"
                 (concat "* decline ~verb~\n  de·cline\n** intransitive verb\n"
                         "   4.\n      a. to sink\n         the declining sun\n")
                 'dictionary))
          (with-current-buffer mw-thesaurus-buffer-name
            (should org-indent-mode)
            (should (equal shown (buffer-string)))
            (search-forward "the declining")
            (goto-char (match-beginning 0)))
          (mw-thesaurus-test--pages '("sun" "* sun ~noun~\n"))
          (with-current-buffer mw-thesaurus-buffer-name
            (mw-thesaurus-back)
            ;; The page comes back as it was shown, not unindented twice.
            (should (equal shown (buffer-string)))
            (should (looking-at-p "the declining sun"))))
      (remove-hook 'org-mode-hook #'org-indent-mode)
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
