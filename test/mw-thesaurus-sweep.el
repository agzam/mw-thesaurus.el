;;; mw-thesaurus-sweep.el --- Look a word list up in batch and compare versions -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;  `mw-sweep-run' looks each word up with the mw-thesaurus on load-path and
;;  writes what showed; `mw-sweep-compare' reads the files of origin/master
;;  and the checkout and prints what differs between them or looks broken.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'org-lint)

(defconst mw-sweep--dir (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the test files.")

(load (expand-file-name "mw-thesaurus-e2e.el" mw-sweep--dir) nil t)

(defvar mw-sweep--events nil
  "What the current lookup did, newest first: (TIME KIND TEXT).")

(defun mw-sweep--log (kind text)
  "Record an event of KIND with TEXT."
  (push (list (float-time) kind text) mw-sweep--events))

(defun mw-sweep--texts (kind)
  "Return the texts of the events of KIND, oldest first."
  (mapcar #'caddr (seq-filter (lambda (e) (eq kind (cadr e))) (reverse mw-sweep--events))))

(defun mw-sweep--setup ()
  "Load mw-thesaurus and record what each lookup does."
  (require 'mw-thesaurus)
  (load (expand-file-name "mw-thesaurus-e2e-cache.el" mw-sweep--dir) nil t)
  (setq mw-e2e-cache-log #'mw-sweep--log)
  ;; A batch frame has no room for the page's window.
  (advice-add 'switch-to-buffer-other-window :override #'set-buffer)
  (advice-add 'message :around
              (lambda (_ format-string &rest args)
                (when format-string
                  (let ((text (apply #'format-message format-string args)))
                    (mw-sweep--log 'echo text)
                    text))))
  (advice-add 'url-retrieve :filter-args
              (lambda (args)
                (let ((callback (nth 1 args)))
                  (append (list (car args)
                                (lambda (&rest status)
                                  (condition-case err
                                      (apply callback status)
                                    (error (mw-sweep--log 'error (error-message-string err))))))
                          (nthcdr 2 args)))))
  (advice-add 'mw-thesaurus--create-buffer :around
              (lambda (orig &rest args)
                (let ((start (float-time)))
                  (apply orig args)
                  (mw-sweep--log 'render (- (float-time) start))))))

(defun mw-sweep--settled ()
  "Non-nil once the lookup showed a page or a message and went quiet."
  (let ((kinds (mapcar #'cadr mw-sweep--events))
        (pending (seq-some (lambda (text) (string-match-p "showing" text))
                           (mw-sweep--texts 'echo))))
    (and (or (memq 'render kinds)
             (and (not pending) (or (memq 'echo kinds) (memq 'error kinds))))
         (< 0.5 (- (float-time) (car (car mw-sweep--events)))))))

(defun mw-sweep--counting-p (bullets)
  "Non-nil when BULLETS, such as (\"1.\" \"2.\") or (\"a)\" \"b)\"), count up by one."
  (let ((codes (mapcar (lambda (bullet)
                         (if (string-match "\\`[0-9]+" bullet)
                             (string-to-number (match-string 0 bullet))
                           (aref bullet 0)))
                       bullets)))
    (cl-loop for (a b) on codes while b always (= b (1+ a)))))

(defun mw-sweep--list-problems ()
  "Return the ordered lists of the page whose bullets do not count up by one."
  (let (problems)
    (org-element-map (org-element-parse-buffer) 'plain-list
      (lambda (plain-list)
        (when (eq 'ordered (org-element-property :type plain-list))
          (let ((bullets (mapcar (lambda (item)
                                   (string-trim (org-element-property :bullet item)))
                                 (org-element-contents plain-list))))
            (unless (mw-sweep--counting-p bullets)
              (push (cons (line-number-at-pos (org-element-property :begin plain-list))
                          bullets)
                    problems))))))
    (nreverse problems)))

(defun mw-sweep--lint ()
  "Return org-lint's reports on the page as (LINE . MESSAGE)."
  (mapcar (lambda (report)
            (let ((fields (cadr report)))
              (cons (aref fields 0) (aref fields 2))))
          (org-lint)))

(defun mw-sweep--lookup (word reference)
  "Look WORD up in REFERENCE, nil for a call without one, and return what showed."
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    (kill-buffer buffer))
  (setq mw-sweep--events nil)
  (let ((start (float-time)))
    (condition-case err
        (if reference
            (mw-thesaurus--lookup-word word reference)
          (mw-thesaurus--lookup-word word))
      (error (mw-sweep--log 'error (error-message-string err))))
    (while (and (< (- (float-time) start) 30) (not (mw-sweep--settled)))
      (accept-process-output nil 0.05))
    (let ((buffer (get-buffer mw-thesaurus-buffer-name)))
      (list :word word
            :reference (or reference 'thesaurus)
            :text (when buffer
                    (with-current-buffer buffer
                      (buffer-substring-no-properties (point-min) (point-max))))
            :echo (mw-sweep--texts 'echo)
            :errors (mw-sweep--texts 'error)
            :render (apply #'+ (mw-sweep--texts 'render))
            :seconds (- (float-time) start)
            :fetches (length (mw-sweep--texts 'fetch))
            :lint (when buffer (with-current-buffer buffer (mw-sweep--lint)))
            :lists (when buffer (with-current-buffer buffer (mw-sweep--list-problems)))))))

(defun mw-sweep--words (file)
  "Return the words of FILE, one a line; # starts a comment."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (seq-remove #'string-empty-p
                (mapcar (lambda (line) (string-trim (replace-regexp-in-string "#.*" "" line)))
                        (split-string (buffer-string) "\n")))))

(defun mw-sweep-run (words-file out-file references)
  "Look up the words of WORDS-FILE in REFERENCES and write what showed to OUT-FILE.
A nil reference calls the lookup without one, as origin/master takes it."
  (mw-sweep--setup)
  (let (records)
    (dolist (word (mw-sweep--words words-file))
      (dolist (reference references)
        (let ((record (mw-sweep--lookup word reference)))
          (push record records)
          (princ (format "%-32s %-10s %5.2fs %s\n" word (plist-get record :reference)
                         (plist-get record :seconds)
                         (if-let* ((text (plist-get record :text)))
                             (car (split-string text "\n"))
                           (string-join (plist-get record :echo) " | ")))))))
    (setq records (nreverse records))
    (let ((coding-system-for-write 'utf-8)
          (print-length nil)
          (print-level nil))
      (with-temp-file out-file
        (prin1 records (current-buffer))))
    (princ (format "%d lookups, %d API requests\n" (length records)
                   (apply #'+ (mapcar (lambda (r) (plist-get r :fetches)) records))))))

(defconst mw-sweep--suspects
  '(("raw API token" . "[{}]")
    ("Lisp value" . "#<\\|\\_<nil\\_>\\|:json-false\\|\\\\n")
    ("empty heading" . "^\\*+[ \t]*$")
    ("Org fixed-width line" . "^[ \t]*:\\( \\|$\\)")
    ("space before punctuation" . "[^ \n] +[,.;!?)]")
    ("double space" . "[^ \n]  +[^ \n]")
    ("empty brackets" . "()\\|\\[\\]")
    ("trailing space" . "[ \t]+$"))
  "Patterns of text that a page should not hold: (NAME . REGEXP).")

(defun mw-sweep--read (file)
  "Return the records in FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (read (current-buffer))))

(defun mw-sweep--suspect-lines (text)
  "Return (NAME LINE-NUMBER LINE) for each line of TEXT a suspect pattern matches."
  (let ((lines (split-string text "\n"))
        found)
    (cl-loop for line in lines for n from 1
             do (dolist (suspect mw-sweep--suspects)
                  (when (string-match-p (cdr suspect) line)
                    (push (list (car suspect) n line) found))))
    (nreverse found)))

(defun mw-sweep-compare (main-file branch-file)
  "Print how the lookups in BRANCH-FILE differ from MAIN-FILE and what looks broken."
  (let* ((main (mw-sweep--read main-file))
         (branch (mw-sweep--read branch-file))
         (counts (list :texts 0 :echoes 0 :slower 0 :errors 0 :suspects 0 :lint 0 :lists 0)))
    (cl-flet ((count (key) (setq counts (plist-put counts key (1+ (plist-get counts key))))))
      (dolist (new branch)
        (let* ((word (plist-get new :word))
               (reference (plist-get new :reference))
               (old (seq-find (lambda (r) (equal word (plist-get r :word))) main)))
          (when (plist-get new :errors)
            (count :errors)
            (princ (format "ERROR %s %s: %S\n" reference word (plist-get new :errors))))
          (when (and old (eq reference 'thesaurus))
            (let ((old-text (or (plist-get old :text) ""))
                  (new-text (or (plist-get new :text) "")))
              (when-let* ((diff (mw-e2e--page-diff
                                 (list :thesaurus (list :head (car (split-string old-text "\n"))
                                                        :text old-text))
                                 (list :thesaurus (list :head (car (split-string new-text "\n"))
                                                        :text new-text)))))
                (count :texts)
                (princ (format "TEXT %s\n" word))
                (dolist (line (seq-take (split-string diff "\n" t) 40))
                  (princ (format "    %s\n" line))))
              (unless (equal (plist-get old :echo) (plist-get new :echo))
                (count :echoes)
                (princ (format "ECHO %s: %S -> %S\n" word
                               (plist-get old :echo) (plist-get new :echo))))
              (when (< (+ 0.05 (* 1.5 (plist-get old :render))) (plist-get new :render))
                (count :slower)
                (princ (format "SLOWER %s: %.3fs -> %.3fs\n" word
                               (plist-get old :render) (plist-get new :render))))))
          (when (eq reference 'dictionary)
            (when-let* ((text (plist-get new :text)))
              (dolist (found (mw-sweep--suspect-lines text))
                (count :suspects)
                (princ (format "SUSPECT %s, %s line %d: %s\n"
                               (car found) word (cadr found) (caddr found)))))
            (dolist (report (plist-get new :lint))
              (count :lint)
              (princ (format "LINT %s line %s: %s\n" word (car report) (cdr report))))
            (dolist (problem (plist-get new :lists))
              (count :lists)
              (princ (format "LIST %s line %d: %S\n" word (car problem) (cdr problem))))))))
    (princ (format (concat "SWEEP %d main, %d branch lookups: %d TEXT, %d ECHO, %d SLOWER, "
                           "%d ERROR, %d SUSPECT, %d LINT, %d LIST\n")
                   (length main) (length branch)
                   (plist-get counts :texts) (plist-get counts :echoes)
                   (plist-get counts :slower) (plist-get counts :errors)
                   (plist-get counts :suspects) (plist-get counts :lint)
                   (plist-get counts :lists)))))

(provide 'mw-thesaurus-sweep)
;;; mw-thesaurus-sweep.el ends here
