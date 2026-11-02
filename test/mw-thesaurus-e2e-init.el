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

(defun mw-e2e-defaults ()
  "Return the values a case may change, for the reset to restore.
1.1.0 keeps the URL in `mw-thesaurus--base-url', later versions in
`mw-thesaurus--api-url'."
  (mapcar (lambda (var) (cons var (symbol-value var)))
          (seq-filter #'boundp '(mw-thesaurus-api-key mw-thesaurus--base-url
                                 mw-thesaurus--api-url))))

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

(advice-add 'mw-thesaurus--create-buffer :after
            (lambda (dict-str &rest _)
              (mw-e2e--log 'render (car (split-string dict-str "\n"))))
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
  (dolist (var mw-e2e-defaults) (set (car var) (cdr var)))
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
                    :mode major-mode
                    :mw (bound-and-true-p mw-thesaurus-mode)
                    :read-only buffer-read-only
                    :point (if-let* ((w (get-buffer-window thesaurus)))
                               (window-point w)
                             (point)))))
          :origin (when origin
                    (with-current-buffer origin
                      (list :text-ok (equal (buffer-string) mw-e2e-origin-text)
                            :point (point)
                            :mark-active mark-active)))
          :lookups (mw-e2e--texts 'lookup)
          :outcomes (mw-e2e--texts 'outcome)
          :errors (mw-e2e--texts 'error)
          :renders (length (seq-filter (lambda (e) (eq (nth 1 e) 'render)) mw-e2e-events))
          :hook mw-e2e-hook-ran
          :minibuffer (and (active-minibuffer-window) t)
          :http-buffers (seq-count (lambda (b) (string-match-p "\\` \\*\\(http\\|request\\)"
                                                                (buffer-name b)))
                                   (buffer-list))
          :asking (mw-e2e-processes))))

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
