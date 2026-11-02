;;; mw-thesaurus-e2e.el --- Keystrokes into emacs -nw -q under tmux, against the real API -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;  Types each case into terminal Emacsen under a private tmux server and
;;  reads what it left through each Emacs's server.  Every case runs in the
;;  checkout and in origin/master's mw-thesaurus; the run prints each
;;  difference.  MW_E2E_PROFILES and MW_E2E_CASES narrow a run.
;;
;;; Code:

(require 'cl-lib)
(require 'package)
(require 'server)
(require 'subr-x)

(defconst mw-e2e--test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the E2E files.")

(defconst mw-e2e--root
  (file-name-directory (directory-file-name mw-e2e--test-dir))
  "The checkout.")

(defconst mw-e2e--dir (file-name-as-directory (expand-file-name ".e2e" mw-e2e--root))
  "Where a run keeps its packages, sockets and profiles.")

(defconst mw-e2e--profiles
  (split-string (or (getenv "MW_E2E_PROFILES") "vanilla evil") "[ ,]+" t)
  "The profiles of this run.")

(defconst mw-e2e--only (getenv "MW_E2E_CASES")
  "A regexp of the case labels to run, or nil for all.")

(defconst mw-e2e--flavors '("main" "branch")
  "origin/master's package, then the checkout's.")

;;; Cases

(defconst mw-e2e--happy "I feel happy today.\n"
  "A line whose word happy spans 8 to 13.")

(defun mw-e2e--set-url (url)
  "Return a form pointing either version of the package at URL."
  `(setq mw-thesaurus--base-url ,url mw-thesaurus--api-url ,url))

(defun mw-e2e--word-case (label word &rest expect)
  "Return a case typing WORD at the prompt of `mw-thesaurus-lookup'.
LABEL names it; EXPECT is its plist of checks.  A case that does not
say what the thesaurus shows passes whatever entries come back."
  (unless (or (plist-get expect :shows) (plist-get expect :no-thesaurus))
    (error "Case %S expects neither :shows nor :no-thesaurus" label))
  (list label :text "\n" :point 1 :tags '(word)
        :steps `((cmd "mw-thesaurus-lookup") (wait-prompt) (type ,word) (keys "Enter") (wait-done))
        :expect expect))

(defconst mw-e2e--cases
  (append
   `(("dwim, word middle" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :windows 2 :hook t))
     ("dwim, word start" :text ,mw-e2e--happy :point 8 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy "))
     ("dwim, right after the word" :text ,mw-e2e--happy :point 13 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy "))
     ("dwim, whitespace prompts" :text "happy   sad\n" :point 7 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-prompt) (type "tenacity") (keys "Enter") (wait-done))
      :expect (:shows "^\\* tenacity "))
     ("at-point command" :text "The weakest link.\n" :point 6 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-at-point") (wait-done))
      :expect (:shows "^\\* weak "))
     ("region, two words" :text "We give up now.\n" :point 4 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f") ("v" "e" "w" "e"))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* give up "))
     ("region, three words" :text "They break the law daily.\n" :point 6 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f" "M-f") ("v" "e" "w" "e" "w" "e"))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* break the law "))
     ("region, trailing space" :text ,mw-e2e--happy :point 8 :tags (flow)
      :steps ((region ("C-Space" "M-f" "Right") ("v" "e" "l"))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy "))
     ("region, across a line break" :text "It is under the\nweather now.\n" :point 7 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f" "M-f") ("v" "e" "w" "e" "w" "e"))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* under the weather "))
     ("region, whole line" :text "happy\nsad\n" :point 1 :tags (flow)
      :steps ((region ("C-Space" "C-n") ("V"))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy "))
     ("drill down with C-c C-o" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "C-c" "C-o") (wait-done))
      :expect (:shows "^\\* glad " :windows 2 :lookups 2))
     ("drill down with RET" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6))
      :expect (:shows "^\\* glad " :windows 2 :lookups 2))
     ("drill down with dwim" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* glad " :windows 2))
     ("q quits" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "q") (pause 1) (keys "C-g") (pause 1))
      :expect (:no-thesaurus t))
     ("reopen after q" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "q") (pause 1) (keys "C-g")
              (eval (select-window (get-buffer-window "e2e.txt")))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :windows 2))
     ("read-only buffer" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (with-current-buffer "e2e.txt" (read-only-mode 1)))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy "))
     ("frame already split" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (let ((other (split-window-right)))
                      (set-window-buffer other (get-buffer-create "*scratch*"))))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :windows 2))
     ("empty prompt" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (keys "Enter") (wait-done 8))
      :expect (:no-thesaurus t :echo "Word is required"))
     ("C-g at the prompt" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (keys "C-g") (pause 2))
      :expect (:no-thesaurus t :lookups 0))
     ("two lookups at once" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter")
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "sad") (keys "Enter")
              (wait-done 20 2.5))
      :expect (:windows 2 :lookups 2))
     ("answer lands during M-x" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((keys "M-x") (wait-prompt)
              (eval (run-at-time 0 nil #'mw-thesaurus--lookup-word "happy"))
              (wait-done) (type "ignore") (keys "Enter") (pause 1))
      :expect (:shows "^\\* happy ")))
   (list
    (mw-e2e--word-case "word happy" "happy" :shows "^\\* happy " :only "happy")
    (mw-e2e--word-case "word Happy" "Happy" :shows "^\\* happy " :only "happy")
    (mw-e2e--word-case "word HAPPY" "HAPPY" :shows "^\\* happy " :only "happy")
    (mw-e2e--word-case "word weakest" "weakest" :shows "^\\* weak " :only "^\\* weak ")
    (mw-e2e--word-case "word running" "running"
                       :shows "^\\* running " :only "\\<\\(running\\|run\\|ran\\)\\>")
    (mw-e2e--word-case "word went" "went" :shows "^\\* went " :only "\\<\\(went\\|go\\)\\>")
    (mw-e2e--word-case "word tenacity" "tenacity" :shows "^\\* tenacity " :only "tenacity")
    (mw-e2e--word-case "word run" "run" :shows "^\\* run " :only "\\<\\(run\\|ran\\)\\>")
    (mw-e2e--word-case "word good" "good" :shows "^\\* good " :only "\\<[Gg]ood\\>")
    (mw-e2e--word-case "word Both" "Both" :no-thesaurus t
                       :echo "No Merriam-Webster entry for \"Both\"")
    (mw-e2e--word-case "word the" "the" :no-thesaurus t)
    (mw-e2e--word-case "word and" "and" :no-thesaurus t)
    (mw-e2e--word-case "word neither" "neither" :shows "^\\* neither " :only "neither")
    (mw-e2e--word-case "word is" "is" :shows "^\\* is " :only "\\<\\(is\\|be\\)\\>")
    (mw-e2e--word-case "typo hapy" "hapy" :shows "^\\* happy " :echo "showing \"happy\"")
    (mw-e2e--word-case "typo HAPY" "HAPY" :shows "^\\* happy ")
    (mw-e2e--word-case "typo recieve" "recieve" :shows "^\\* receive ")
    (mw-e2e--word-case "typo definately" "definately"
                       :shows "^\\* definitely " :only "\\<definite")
    (mw-e2e--word-case "typo seperate" "seperate"
                       :shows "^\\* separate " :only "separate" :echo "showing \"separate\"")
    (mw-e2e--word-case "missing etymology" "etymology" :no-thesaurus t)
    (mw-e2e--word-case "missing lisp" "lisp" :shows "^\\* lip " :echo "showing \"lip\"")
    (mw-e2e--word-case "gibberish" "xqzvbnwpt" :no-thesaurus t :lookups 1)
    (mw-e2e--word-case "apostrophe don't" "don't" :shows "^\\* don " :echo "showing \"don\"")
    (mw-e2e--word-case "apostrophe can't" "can't" :no-thesaurus t)
    (mw-e2e--word-case "hyphen well-known" "well-known"
                       :shows "^\\* well-known " :only "^\\* well-known ")
    (mw-e2e--word-case "non-ASCII café" "café" :shows "^\\* café " :only "café")
    (mw-e2e--word-case "non-ASCII naïve" "naïve" :shows "^\\* naive ")
    (mw-e2e--word-case "non-ASCII résumé" "résumé" :shows "^\\* resume ")
    (mw-e2e--word-case "spaces around" "  happy  " :shows "^\\* happy ")
    (mw-e2e--word-case "punctuation happy!" "happy!" :shows "^\\* happy ")
    (mw-e2e--word-case "phrase under the weather" "under the weather"
                       :shows "^\\* under the weather ")
    (mw-e2e--word-case "phrase giv up" "giv up" :shows "^\\* give up " :only "give up")
    (mw-e2e--word-case "sentence" "the quick brown fox jumps over the lazy dog"
                       :no-thesaurus t)
    (mw-e2e--word-case "single letter" "a" :no-thesaurus t)
    (mw-e2e--word-case "number" "123" :no-thesaurus t))
   `(("bad key" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-api-key "bad-e2e-key"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done))
      :expect (:no-thesaurus t :echo "Invalid API key"))
     ("unknown host" :text "\n" :point 1 :tags (env)
      :steps ((eval ,(mw-e2e--set-url "https://mw-e2e.invalid/thesaurus/"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :error "."))
     ("refused connection" :text "\n" :point 1 :tags (env)
      :steps ((eval ,(mw-e2e--set-url "https://127.0.0.1:9/thesaurus/"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :echo "request failed"))
     ("HTTP 404" :text "\n" :point 1 :tags (env)
      :steps ((eval ,(mw-e2e--set-url
                      "https://www.dictionaryapi.com/api/v3/references/thesaurus/jsonx/"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :echo "request failed"))
     ("upgrade in a running session" :text "\n" :point 1 :tags (env upgrade)
      :steps ((eval (let ((inhibit-message t))
                      (load (expand-file-name "mw-thesaurus.el" ,mw-e2e--root) nil t)
                      (setq mw-e2e-defaults (mw-e2e-defaults))))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done))
      :expect (:shows "^\\* happy "))))
  "The cases: (LABEL :text :point :tags :steps :expect).
Steps are (cmd NAME), (keys KEY...) in tmux names, (type TEXT),
\(region VANILLA-KEYS EVIL-KEYS), (search TEXT), (eval FORM),
\(wait-prompt), (wait-done [SECONDS QUIET]) and (pause SECONDS).")

(defun mw-e2e--selected-cases (profile)
  "Return the cases PROFILE runs: all for vanilla, the flow cases for evil."
  (seq-filter (lambda (case)
                (and (or (null mw-e2e--only) (string-match-p mw-e2e--only (car case)))
                     (or (equal profile "vanilla")
                         (memq 'flow (plist-get (cdr case) :tags)))))
              mw-e2e--cases))

;;; Processes

(defun mw-e2e--sh (program &rest args)
  "Run PROGRAM with ARGS; return its output, signal an error when it fails."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (eql status 0)
        (error "%s %s: %s" program (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun mw-e2e--tmux (&rest args)
  "Run tmux with ARGS on the private server of the run, without ~/.tmux.conf."
  (apply #'mw-e2e--sh "tmux" "-f" "/dev/null"
         "-S" (expand-file-name "tmux.sock" mw-e2e--dir) args))

(defun mw-e2e--eval (name form &optional seconds)
  "Evaluate FORM in the Emacs NAME; signal an error after SECONDS, 20 by default."
  (let ((server-socket-dir (expand-file-name "sock/" mw-e2e--dir)))
    (with-timeout ((or seconds 20) (error "No answer from %s" name))
      (server-eval-at name form))))

(defun mw-e2e--packages ()
  "Install what origin/master and the evil profile need into the run's ELPA."
  (setq package-user-dir (expand-file-name "elpa" mw-e2e--dir))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (when-let* ((missing (seq-remove #'package-installed-p '(request dash evil))))
    (package-refresh-contents)
    (mapc #'package-install missing)))

(defun mw-e2e--main-source ()
  "Write origin/master's mw-thesaurus.el into the run and return its directory."
  (let ((dir (expand-file-name "main/" mw-e2e--dir))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (make-directory dir t)
    (with-temp-file (expand-file-name "mw-thesaurus.el" dir)
      (let ((default-directory mw-e2e--root))
        (unless (eql 0 (call-process "git" nil t nil "show" "origin/master:mw-thesaurus.el"))
          (error "Cannot read origin/master:mw-thesaurus.el"))))
    dir))

(defun mw-e2e--start (name profile source)
  "Start the Emacs NAME of PROFILE with mw-thesaurus from SOURCE.
Return what it reports about itself."
  (let ((sockets (expand-file-name "sock/" mw-e2e--dir))
        (home (expand-file-name "home/" mw-e2e--dir))
        (init-dir (expand-file-name (concat "profiles/" name "/") mw-e2e--dir)))
    (make-directory sockets t)
    (set-file-modes sockets #o700)
    (make-directory home t)
    (when (file-exists-p init-dir) (delete-directory init-dir t))
    (make-directory init-dir t)
    (ignore-errors (delete-file (expand-file-name name sockets)))
    (ignore-errors (mw-e2e--tmux "kill-session" "-t" (concat "=" name)))
    (apply #'mw-e2e--tmux "new-session" "-d" "-s" name "-x" "120" "-y" "40" "-c" home
           "env" "-i"
           (concat "HOME=" home)
           (concat "PATH=" (getenv "PATH"))
           (concat "TMPDIR=" (or (getenv "TMPDIR") "/tmp"))
           (concat "USER=" (or (getenv "USER") "e2e"))
           "LANG=en_US.UTF-8" "TERM=screen-256color"
           (concat "MW_E2E_ELPA=" (expand-file-name "elpa" mw-e2e--dir))
           (concat "MW_E2E_SOURCE=" source)
           (concat "MW_E2E_PROFILE=" profile)
           (concat "MW_E2E_SOCKET_DIR=" sockets)
           (concat "MW_E2E_SERVER=" name)
           (list (expand-file-name invocation-name invocation-directory)
                 "-nw" "-q" "--init-directory" init-dir
                 "-l" (expand-file-name "mw-thesaurus-e2e-init.el" mw-e2e--test-dir)))
    (let ((end (+ (float-time) 30)))
      (while (and (not (file-exists-p (expand-file-name name sockets)))
                  (< (float-time) end))
        (sleep-for 0.25)))
    (mw-e2e--eval name '(mw-e2e-hello))))

(defun mw-e2e--stop (name)
  "Stop the Emacs NAME."
  (ignore-errors (mw-e2e--eval name '(run-at-time 0.1 nil #'kill-emacs 0) 5))
  (sleep-for 0.3)
  (ignore-errors (mw-e2e--tmux "kill-session" "-t" (concat "=" name))))

;;; Typing

(defun mw-e2e--keys (name &rest keys)
  "Send KEYS, tmux key names, to the Emacs NAME."
  (dolist (key keys)
    (mw-e2e--tmux "send-keys" "-t" (concat "=" name ":") key)
    (sleep-for 0.05)))

(defun mw-e2e--type (name text)
  "Type TEXT into the Emacs NAME."
  (mw-e2e--tmux "send-keys" "-t" (concat "=" name ":") "-l" "--" text))

(defun mw-e2e--poll (name form seconds)
  "Wait until FORM is non-nil in the Emacs NAME; return nil, or a note after SECONDS."
  (let ((end (+ (float-time) seconds)) done)
    (while (and (not (setq done (ignore-errors (mw-e2e--eval name form 5))))
                (< (float-time) end))
      (sleep-for 0.25))
    (unless done (format "%s not true after %ss" form seconds))))

(defun mw-e2e--step (name profile step)
  "Do STEP of a case in the Emacs NAME of PROFILE; return a note or nil."
  (pcase step
    (`(cmd ,command)
     (mw-e2e--keys name "M-x")
     (or (mw-e2e--poll name '(and (active-minibuffer-window) t) 5)
         (progn (mw-e2e--type name command) (mw-e2e--keys name "Enter") nil)))
    (`(keys . ,keys) (apply #'mw-e2e--keys name keys) nil)
    (`(type ,text) (mw-e2e--type name text) nil)
    (`(region ,vanilla ,evil)
     (apply #'mw-e2e--keys name (if (equal profile "evil") evil vanilla)) nil)
    (`(search ,text)
     (if (equal profile "evil")
         (progn (mw-e2e--keys name "/") (mw-e2e--type name text) (mw-e2e--keys name "Enter"))
       (mw-e2e--keys name "C-s") (mw-e2e--type name text) (mw-e2e--keys name "Enter" "M-b"))
     (sleep-for 0.3) nil)
    ;; The value may be a window or a buffer, which the server cannot print readably.
    (`(eval ,form) (mw-e2e--eval name `(progn ,form t)) nil)
    ('(wait-prompt) (mw-e2e--poll name '(and (active-minibuffer-window) t) 5))
    (`(wait-done . ,args)
     (mw-e2e--poll name `(mw-e2e-settled ,(or (cadr args) 1.2)) (or (car args) 15)))
    (`(pause ,seconds) (sleep-for seconds) nil)
    (_ (error "Unknown step %S" step))))

(defun mw-e2e--close-prompts (name)
  "Quit any prompt the Emacs NAME has open.
A C-g reaches a prompt only from its window, which a lookup may have left."
  (dotimes (_ 3)
    (when (ignore-errors (mw-e2e--eval name '(and (active-minibuffer-window) t) 5))
      (ignore-errors
        (mw-e2e--eval name '(progn (select-window (active-minibuffer-window)) t) 5))
      (ignore-errors (mw-e2e--keys name "C-g"))
      (sleep-for 0.3))))

(defun mw-e2e--run-case (name profile case)
  "Run CASE in the Emacs NAME of PROFILE; return (SECONDS NOTES STATE)."
  (let ((start (float-time))
        notes)
    (mw-e2e--close-prompts name)
    (condition-case err
        (progn
          (mw-e2e--eval name `(mw-e2e-reset ,(plist-get (cdr case) :text)
                                            ,(plist-get (cdr case) :point)))
          (dolist (step (plist-get (cdr case) :steps))
            (when-let* ((note (mw-e2e--step name profile step)))
              (push note notes))))
      (error (push (format "driver: %s" (error-message-string err)) notes)))
    (let ((state (condition-case err (mw-e2e--eval name '(mw-e2e-state))
                   (error (push (format "no state: %s" (error-message-string err)) notes)
                          nil))))
      ;; A prompt or a pending key sequence left behind would eat the next case.
      (ignore-errors (mw-e2e--keys name "C-g"))
      (mw-e2e--close-prompts name)
      (list (round (- (float-time) start)) (nreverse notes) state))))

;;; Checks

(defun mw-e2e--outcome (state)
  "Return the line that sums up STATE."
  (if (null state)
      "no state"
    (let ((thesaurus (plist-get state :thesaurus)))
      (string-join
       (delq nil
             (list (if thesaurus
                       (format "shows %S (%d entries)" (plist-get thesaurus :head)
                               (plist-get thesaurus :entries))
                     "no thesaurus")
                   (when-let* ((outcomes (plist-get state :outcomes)))
                     (format "echo %S" (string-join outcomes " | ")))
                   (when-let* ((errors (plist-get state :errors)))
                     (format "error %S" (string-join errors " | ")))
                   (format "lookups %S" (plist-get state :lookups))
                   (format "windows %d, selected %s" (length (plist-get state :windows))
                           (plist-get state :selected))))
       "; "))))

(defun mw-e2e--failures (state expect)
  "Return what in STATE breaks the checks every case has, or EXPECT."
  (let ((thesaurus (plist-get state :thesaurus))
        (origin (plist-get state :origin))
        failures)
    (cl-flet ((fail (fmt &rest args) (push (apply #'format fmt args) failures)))
      (when (null state) (fail "no state"))
      (when (and origin (not (plist-get origin :text-ok)))
        (fail "the text of e2e.txt changed"))
      (when thesaurus
        (unless (eq (plist-get thesaurus :mode) 'org-mode) (fail "thesaurus not in org-mode"))
        (unless (plist-get thesaurus :mw) (fail "mw-thesaurus-mode off"))
        (unless (plist-get thesaurus :read-only) (fail "thesaurus buffer writable")))
      (when (plist-get state :minibuffer) (fail "a prompt is left open"))
      (when (and (plist-get state :errors) (not (plist-get expect :error)))
        (fail "unexpected error: %s" (string-join (plist-get state :errors) " | ")))
      (when-let* ((re (plist-get expect :error)))
        (unless (seq-some (lambda (e) (string-match-p re e)) (plist-get state :errors))
          (fail "no error matching %S" re)))
      (when-let* ((re (plist-get expect :shows)))
        (unless (and thesaurus (string-match-p re (plist-get thesaurus :head)))
          (fail "expected the thesaurus to show %S" re)))
      (when-let* ((re (plist-get expect :only)))
        (dolist (head (plist-get thesaurus :heads))
          (unless (string-match-p re head)
            (fail "entry %S does not match %S" head re))))
      (when-let* ((re (plist-get expect :echo)))
        (unless (seq-some (lambda (m) (string-match-p re m)) (plist-get state :outcomes))
          (fail "no echo matching %S" re)))
      (when (and (plist-get expect :no-thesaurus) thesaurus)
        (fail "expected no thesaurus buffer"))
      (when-let* ((n (plist-get expect :windows)))
        (unless (= n (length (plist-get state :windows)))
          (fail "expected %d windows" n)))
      (when-let* ((n (plist-get expect :lookups)))
        (unless (= n (length (plist-get state :lookups)))
          (fail "expected %d lookups" n)))
      (when (and (plist-get expect :hook) (not (plist-get state :hook)))
        (fail "mw-thesaurus-mode-hook did not run")))
    (nreverse failures)))

;;; Runs

(defvar mw-e2e--totals (list :cases 0 :failures 0 :same 0 :fixed 0 :diffs 0 :requests 0)
  "What the run counted.")

(defun mw-e2e--count (key &optional n)
  "Add N, 1 by default, to the total KEY."
  (setq mw-e2e--totals (plist-put mw-e2e--totals key (+ (or n 1) (plist-get mw-e2e--totals key)))))

(defun mw-e2e--profile (profile)
  "Run the cases of PROFILE in origin/master's package and in the checkout's."
  (let* ((names (mapcar (lambda (flavor) (concat "mw-" profile "-" flavor)) mw-e2e--flavors))
         (sources (list (mw-e2e--main-source) mw-e2e--root)))
    (cl-loop for name in names for source in sources
             do (princ (format "start %s: %S\n" name (mw-e2e--start name profile source))))
    (unwind-protect
        (dolist (case (mw-e2e--selected-cases profile))
          (let (outcomes failures)
            (cl-loop
             for name in names for flavor in mw-e2e--flavors
             do (pcase-let* ((`(,seconds ,notes ,state) (mw-e2e--run-case name profile case))
                             (outcome (mw-e2e--outcome state)))
                  (push outcome outcomes)
                  (push (append notes (mw-e2e--failures state (plist-get (cdr case) :expect)))
                        failures)
                  (mw-e2e--count :requests (length (plist-get state :lookups)))
                  (princ (format "%-7s %-6s %-30s %2ss => %s\n" profile flavor (car case)
                                 seconds outcome))))
            (mw-e2e--count :cases)
            ;; FAIL is new in the branch, SAME fails in both, FIXED only in main.
            ;; The upgrade case loads the checkout into main's Emacs, so main's
            ;; failures there are what an upgrade from 1.1.0 meets.
            (pcase-let ((`(,branch ,main) failures))
              (when (memq 'upgrade (plist-get (cdr case) :tags))
                (setq branch (append main branch) main nil))
              (dolist (failure branch)
                (if (member failure main)
                    (progn (mw-e2e--count :same) (princ (format "  SAME %s\n" failure)))
                  (mw-e2e--count :failures) (princ (format "  FAIL %s\n" failure))))
              (dolist (failure main)
                (unless (member failure branch)
                  (mw-e2e--count :fixed) (princ (format "  FIXED %s\n" failure)))))
            (unless (equal (car outcomes) (cadr outcomes))
              (mw-e2e--count :diffs)
              (princ (format "  DIFF %s\n" (car case))))))
      (cl-loop for name in names
               do (princ (format "%s asks before exit about: %S\n" name
                                 (ignore-errors (mw-e2e--eval name '(mw-e2e-processes))))))
      (mapc #'mw-e2e--stop names))))

(defun mw-e2e-main ()
  "Run every profile of `mw-e2e--profiles' and exit non-zero on a failure."
  (make-directory mw-e2e--dir t)
  (mw-e2e--packages)
  (unwind-protect
      (dolist (profile mw-e2e--profiles)
        (mw-e2e--profile profile))
    (ignore-errors (mw-e2e--tmux "kill-server")))
  (princ (format "SUMMARY %d cases: %d FAIL, %d SAME, %d FIXED, %d DIFF, %d requests\n"
                 (plist-get mw-e2e--totals :cases) (plist-get mw-e2e--totals :failures)
                 (plist-get mw-e2e--totals :same) (plist-get mw-e2e--totals :fixed)
                 (plist-get mw-e2e--totals :diffs) (plist-get mw-e2e--totals :requests)))
  (kill-emacs (if (zerop (plist-get mw-e2e--totals :failures)) 0 1)))

(provide 'mw-thesaurus-e2e)
;;; mw-thesaurus-e2e.el ends here
