;;; mw-thesaurus-e2e-init.el --- Init of each emacs -nw -q the E2E types into -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;  Loads one flavor of mw-thesaurus, starts a server for the driver,
;;  test/mw-thesaurus-e2e.el, and records what each case shows.
;;
;;; Code:

(require 'server)
(require 'package)
(require 'seq)
(require 'subr-x)

(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      package-user-dir (getenv "MW_E2E_ELPA"))
(package-initialize)
(add-to-list 'load-path (getenv "MW_E2E_SOURCE"))
(require 'mw-thesaurus)
(when (equal (getenv "MW_E2E_PROFILE") "evil")
  (require 'evil)
  (evil-mode 1))

(defvar mw-e2e-events nil
  "What the current case did, newest first: (TIME KIND TEXT).")

(defvar mw-e2e-origin-text nil
  "The text the current case put in the buffer e2e.txt.")

(defvar mw-e2e-hook-ran nil
  "Non-nil once `mw-thesaurus-mode-hook' ran in the current case.")

(defconst mw-e2e-variables
  '(mw-thesaurus-api-key mw-thesaurus--base-url mw-thesaurus--api-url
    mw-thesaurus-dictionary-api-key mw-thesaurus--dictionary-api-url
    mw-thesaurus-audio-player exec-path)
  "The variables a case may change.
1.1.0 keeps the URL in `mw-thesaurus--base-url', later versions in
`mw-thesaurus--api-url'.  The dictionary came after 1.4.0.")

;; Recordings download under it; a run must not reuse the user's.
(setq temporary-file-directory
      (file-name-as-directory (make-temp-file "mw-e2e-tmp" t)))
(let ((dir temporary-file-directory))
  (add-hook 'kill-emacs-hook (lambda () (delete-directory dir t))))

(defun mw-e2e--audio-dir ()
  "Return the directory the recordings download into."
  (expand-file-name "mw-thesaurus/" temporary-file-directory))

(defun mw-e2e-defaults ()
  "Return the bound `mw-e2e-variables' with their values, for the reset."
  (mapcar (lambda (var) (cons var (symbol-value var)))
          (seq-filter #'boundp mw-e2e-variables)))

(defvar mw-e2e-defaults (mw-e2e-defaults)
  "The values a case may change and the reset restores.")

(defun mw-e2e--log (kind text)
  "Record an event of KIND with TEXT."
  (push (list (float-time) kind text) mw-e2e-events))

(advice-add 'message :around
            (lambda (orig format-string &rest args)
              (when (stringp format-string)
                (let ((text (ignore-errors (apply #'format-message format-string args))))
                  (when (and text (string-match-p "Merriam-Webster\\|REQUEST\\|[Ee]rror" text))
                    (mw-e2e--log 'outcome text))))
              (apply orig format-string args))
            '((name . mw-e2e)))

(advice-add 'mw-thesaurus--lookup-word :before
            (lambda (word &rest _) (mw-e2e--log 'lookup word))
            '((name . mw-e2e)))

(advice-add 'make-process :before
            (lambda (&rest args)
              (when (equal "mw-thesaurus-audio" (plist-get args :name))
                (mw-e2e--log 'play (string-join (plist-get args :command) " "))))
            '((name . mw-e2e)))

;; The Org text is the last string argument in every version of the package.
(advice-add 'mw-thesaurus--create-buffer :after
            (lambda (&rest args)
              (mw-e2e--log 'render (car (split-string (seq-find #'stringp (reverse args))
                                                      "\n"))))
            '((name . mw-e2e)))

(let ((default command-error-function))
  (setq command-error-function
        (lambda (data context caller)
          (unless (memq (car data) '(quit minibuffer-quit))
            (mw-e2e--log 'error (concat (or context "") (error-message-string data))))
          (funcall default data context caller))))

(add-hook 'mw-thesaurus-mode-hook (lambda () (setq mw-e2e-hook-ran t)))

(defun mw-e2e-hello ()
  "Return what the driver checks after the start."
  (list :lib (locate-library "mw-thesaurus")
        :version (with-temp-buffer
                   (insert-file-contents (locate-library "mw-thesaurus.el"))
                   (and (re-search-forward "^;; Version: \\(.*\\)$" nil t)
                        (match-string 1)))
        :evil (bound-and-true-p evil-mode)
        :url (or (bound-and-true-p mw-thesaurus--api-url)
                 (bound-and-true-p mw-thesaurus--base-url))))

(defun mw-e2e-reset (text point)
  "Start a case: one window on e2e.txt holding TEXT, point at POINT."
  ;; A variable this version lacks goes unbound, so a later upgrade's
  ;; defcustom sets it instead of keeping what an earlier case set.
  (dolist (var mw-e2e-variables)
    (if-let* ((default (assq var mw-e2e-defaults)))
        (set var (cdr default))
      (makunbound var)))
  (when (bound-and-true-p xterm-mouse-mode) (xterm-mouse-mode -1))
  (advice-remove 'url-retrieve 'mw-e2e-slow)
  (remove-hook 'org-mode-hook #'org-indent-mode)
  (delete-directory (mw-e2e--audio-dir) t)
  (when (boundp 'mw-thesaurus--entry-recordings)
    (clrhash mw-thesaurus--entry-recordings))
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name)))
    (kill-buffer buffer))
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "e2e.txt"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text))
  (text-mode)
  (setq buffer-read-only nil)
  (goto-char point)
  (deactivate-mark)
  (when (bound-and-true-p evil-mode) (evil-normal-state))
  (setq mw-e2e-origin-text text
        mw-e2e-hook-ran nil
        mw-e2e-events nil)
  (message nil)
  t)

(defun mw-e2e-settled (quiet)
  "Non-nil when the case logged an outcome and nothing for QUIET seconds."
  (and (seq-some (lambda (event) (memq (nth 1 event) '(render outcome error)))
                 mw-e2e-events)
       (< quiet (- (float-time) (car (car mw-e2e-events))))))

(defun mw-e2e-processes ()
  "Return the processes that would make Emacs ask before exiting."
  (mapcar #'process-name
          (seq-filter (lambda (p)
                        (and (process-query-on-exit-flag p)
                             (memq (process-status p) '(run open connect stop))
                             (not (eq (process-type p) 'serial))
                             (not (string-prefix-p "server" (process-name p)))))
                      (process-list))))

(defun mw-e2e-state ()
  "Return what the current case left."
  (let ((thesaurus (get-buffer mw-thesaurus-buffer-name))
        (origin (get-buffer "e2e.txt")))
    (list :selected (buffer-name (window-buffer (selected-window)))
          :windows (mapcar (lambda (w) (buffer-name (window-buffer w)))
                           (window-list nil 'never))
          :thesaurus
          (when thesaurus
            (with-current-buffer thesaurus
              (list :head (buffer-substring-no-properties
                           (point-min) (save-excursion (goto-char (point-min))
                                                       (line-end-position)))
                    :entries (count-matches "^\\* " (point-min) (point-max))
                    :heads (save-excursion
                             (goto-char (point-min))
                             (let (heads)
                               (while (re-search-forward "^\\* .*" nil t)
                                 (push (match-string-no-properties 0) heads))
                               (nreverse heads)))
                    :senses (count-matches "^\\*\\* " (point-min) (point-max))
                    :indented (count-matches "^[ \t]" (point-min) (point-max))
                    :text (buffer-substring-no-properties (point-min) (point-max))
                    :faces (sort (delete-dups
                                  (seq-filter
                                   (lambda (face) (and face (symbolp face)))
                                   (mapcar (lambda (overlay) (overlay-get overlay 'face))
                                           (overlays-in (point-min) (point-max)))))
                                 #'string<)
                    :reference (bound-and-true-p mw-thesaurus--reference)
                    :header (when-let* ((line (mw-e2e--header-line)))
                              (substring-no-properties line))
                    :current (when-let* ((line (mw-e2e--header-line)))
                               (seq-find (lambda (label)
                                           (when-let* ((at (string-search label line)))
                                             (eq 'bold (get-text-property at 'face line))))
                                         '("Thesaurus" "Dictionary")))
                    :mode major-mode
                    :org (and (derived-mode-p 'org-mode) t)
                    :mw (or (bound-and-true-p mw-thesaurus-mode)
                            (and (derived-mode-p 'mw-thesaurus-mode) t))
                    :read-only buffer-read-only
                    :point (mw-e2e--point thesaurus)
                    :after-point (let ((pos (mw-e2e--point thesaurus)))
                                   (buffer-substring-no-properties
                                    pos (min (point-max) (+ pos 40)))))))
          :origin (when origin
                    (with-current-buffer origin
                      (list :text-ok (equal (buffer-string) mw-e2e-origin-text)
                            :point (point)
                            :mark-active mark-active)))
          :lookups (mw-e2e--texts 'lookup)
          :plays (mw-e2e--texts 'play)
          :audio (when (file-directory-p (mw-e2e--audio-dir))
                   (mapcar (lambda (file)
                             (cons (file-name-nondirectory file)
                                   (file-attribute-size (file-attributes file))))
                           (directory-files (mw-e2e--audio-dir) t "\\.mp3\\'")))
          :outcomes (mw-e2e--texts 'outcome)
          :errors (mw-e2e--texts 'error)
          :renders (length (seq-filter (lambda (e) (eq (nth 1 e) 'render)) mw-e2e-events))
          :hook mw-e2e-hook-ran
          :minibuffer (and (active-minibuffer-window) t)
          :http-buffers (seq-count (lambda (b) (string-match-p "\\` \\*\\(http\\|request\\)"
                                                                (buffer-name b)))
                                   (buffer-list))
          :asking (mw-e2e-processes))))

(defun mw-e2e-slow-replies (seconds)
  "Hold each `url-retrieve' answer for SECONDS, until the next case."
  (advice-add 'url-retrieve :filter-args
              (lambda (args)
                (let ((callback (nth 1 args)))
                  (append (list (car args)
                                (lambda (&rest status)
                                  ;; The callback reads the answer from its buffer.
                                  (let ((buffer (current-buffer)))
                                    (run-at-time seconds nil
                                                 (lambda ()
                                                   (with-current-buffer buffer
                                                     (apply callback status)))))))
                          (nthcdr 2 args))))
              '((name . mw-e2e-slow))))

(defun mw-e2e-goto-speaker (n)
  "Put point on the Nth speaker of the thesaurus page, in its window."
  (with-selected-window (get-buffer-window mw-thesaurus-buffer-name)
    (goto-char (point-min))
    (re-search-forward "\\]\\[🔊\\]\\]" nil nil n)
    (goto-char (+ 2 (match-beginning 0)))))

(defun mw-e2e--header-line ()
  "Return the header line of the thesaurus buffer as its window shows it."
  (when-let* ((buffer (get-buffer mw-thesaurus-buffer-name))
              (window (get-buffer-window buffer)))
    (format-mode-line (buffer-local-value 'header-line-format buffer) nil window buffer)))

(defun mw-e2e-header-position (label)
  "Return the screen column and row, from 1, of LABEL in the header line."
  (when-let* ((line (mw-e2e--header-line))
              (at (string-search label line))
              (window (get-buffer-window mw-thesaurus-buffer-name)))
    (cons (+ 1 (window-left-column window) at)
          (+ 1 (window-top-line window)))))

(defun mw-e2e--point (buffer)
  "Return point in BUFFER, as its window shows it when it has one."
  (if-let* ((window (get-buffer-window buffer)))
      (window-point window)
    (with-current-buffer buffer (point))))

(defun mw-e2e--texts (kind)
  "Return the texts of the events of KIND, oldest first."
  (mapcar (lambda (event) (nth 2 event))
          (seq-filter (lambda (event) (eq (nth 1 event) kind))
                      (reverse mw-e2e-events))))

(setq server-socket-dir (getenv "MW_E2E_SOCKET_DIR")
      server-name (getenv "MW_E2E_SERVER"))
(server-start)

(provide 'mw-thesaurus-e2e-init)
;;; mw-thesaurus-e2e-init.el ends here
