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

(defun mw-e2e--env (name)
  "Return the environment variable NAME, or nil when it is unset or empty."
  (let ((value (getenv name)))
    (unless (equal value "") value)))

(defun mw-e2e--load-dir (source)
  "Return the directory to load mw-thesaurus from: SOURCE, or a compiled copy.
MW_E2E_COMPILE, byte or native, compiles a copy, so the checkout gets no .elc."
  (if-let* ((how (mw-e2e--env "MW_E2E_COMPILE")))
      (let* ((dir (file-name-as-directory
                   (expand-file-name (concat "compiled/" (getenv "MW_E2E_SERVER"))
                                     (getenv "MW_E2E_DIR"))))
             (file (expand-file-name "mw-thesaurus.el" dir)))
        (make-directory dir t)
        (copy-file (expand-file-name "mw-thesaurus.el" source) file t)
        (if (equal how "native")
            (load (native-compile file) nil t)
          (byte-compile-file file))
        dir)
    source))

(defun mw-e2e--evil ()
  "Turn on evil."
  (require 'evil)
  (evil-mode 1))

(defun mw-e2e--right-window-rule ()
  "Show the buffer on the right, as the readme's `use-package' example does."
  (add-to-list 'display-buffer-alist
               `(,mw-thesaurus-buffer-name
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . right) (window . root) (window-width . 0.3))))

(defun mw-e2e--profile (profile)
  "Configure this Emacs as PROFILE, after mw-thesaurus loads.
Each profile copies the readme or a public config that sets up mw-thesaurus."
  (pcase profile
    ("evil" (mw-e2e--evil))
    ("readme"
     (add-hook 'mw-thesaurus-mode-hook #'variable-pitch-mode)
     (mw-e2e--right-window-rule))
    ;; Doom's RET in Org runs +org/dwim-at-point; the 1.4.0 readme remaps it.
    ("doom"
     (mw-e2e--evil)
     (require 'evil-org)
     (add-hook 'org-mode-hook #'evil-org-mode)
     (defalias '+org/dwim-at-point
       (lambda ()
         (interactive)
         (if (org-in-regexp org-link-bracket-re 1)
             (org-open-at-point)
           (message "+org/dwim-at-point")))
       "Doom's RET in Org buffers, as far as a thesaurus page needs it.")
     (evil-define-key '(normal motion) evil-org-mode-map (kbd "RET") #'+org/dwim-at-point)
     (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
     (define-key mw-thesaurus-mode-map [remap +org/dwim-at-point] #'mw-thesaurus-lookup-at-point))
    ;; stubillwhite/.emacs.d, lisp/package-config/sbw-configure-mw-thesaurus.el
    ("stubillwhite"
     (advice-add 'mw-thesaurus-lookup-at-point :around
                 (lambda (orig &rest args)
                   (let ((split-height-threshold nil)
                         (split-width-threshold 0))
                     (apply orig args))))
     (define-key mw-thesaurus-mode-map (kbd "q") 'delete-window))
    ;; a3madkour, M1ndo, fet64, jakebox and MooersLab dotfiles.
    ("evil-local-q"
     (mw-e2e--evil)
     (add-hook 'mw-thesaurus-mode-hook
               (lambda ()
                 (define-key evil-normal-state-local-map (kbd "q") 'mw-thesaurus--quit))))
    ;; younger-1/emacs, lisp/init-tool.el
    ("younger"
     (mw-e2e--evil)
     (define-key mw-thesaurus-mode-map [remap evil-ret] #'mw-thesaurus-lookup-at-point)
     (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
     (add-hook 'mw-thesaurus-mode-hook #'variable-pitch-mode)
     (mw-e2e--right-window-rule))
    ;; mpereira/.emacs.d, init.el
    ("rule-literal"
     (setq-default display-buffer-alist
                   '(("\\* Merriam-Webster Thesaurus \\*" (display-buffer-below-selected)))))
    ;; Jonghyun-Yun/doom-emacs-private's popup rule, as a display rule.
    ("rule-prefix"
     (add-to-list 'display-buffer-alist
                  '("^\\* Merriam-Webster" (display-buffer-in-side-window)
                    (side . right) (window-width . 50))))
    ;; garid3000/My-Emacs-Init, init.org
    ("popper"
     (require 'popper)
     (setq popper-reference-buffers '("\\* Merriam-Webster Thesaurus \\*"))
     (popper-mode 1))
    ("org"
     (setq org-link-descriptive nil
           org-startup-folded t
           org-pretty-entities t)
     ;; Jonghyun-Yun/doom-emacs-private's +write/buffer-face-mode-dict.
     (add-hook 'mw-thesaurus-mode-hook
               (lambda ()
                 (setq buffer-face-mode-face '(:height 1.1))
                 (buffer-face-mode)
                 (setq-local line-spacing 0.5))))))

(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      ;; Such warnings would open a window in the middle of a case.
      native-comp-async-report-warnings-errors 'silent
      package-user-dir (getenv "MW_E2E_ELPA"))
(package-initialize)
(add-to-list 'load-path (mw-e2e--load-dir (getenv "MW_E2E_SOURCE")))
(require 'mw-thesaurus)
(mw-e2e--profile (getenv "MW_E2E_PROFILE"))

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
                  (when (and text (string-match-p
                                   "Merriam-Webster\\|Synonym Chooser\\|REQUEST\\|[Ee]rror" text))
                    (mw-e2e--log 'outcome text))))
              (apply orig format-string args))
            '((name . mw-e2e)))

(advice-add 'mw-thesaurus--lookup-word :before
            (lambda (word &rest _) (mw-e2e--log 'lookup word))
            '((name . mw-e2e)))

;; A case must not open the user's browser.
(setq browse-url-browser-function (lambda (url &rest _) (mw-e2e--log 'browse url)))

(advice-add 'display-warning :before
            (lambda (type message &rest _)
              ;; Another package's native compilation warns at its own pace.
              (unless (memq (if (consp type) (car type) type) '(comp native-compiler))
                (mw-e2e--log 'warning (format "%s: %s" type message))))
            '((name . mw-e2e)))

(load (expand-file-name "mw-thesaurus-e2e-cache.el" (file-name-directory load-file-name))
      nil t)
(setq mw-e2e-cache-log #'mw-e2e--log)

;; A switch between tabs may skip `mw-thesaurus--lookup-word' and still ask the API.
(advice-add 'url-retrieve :before
            (lambda (url &rest _)
              (when (string-match-p "\\`https://www\\.dictionaryapi\\.com/api/" url)
                (mw-e2e--log 'request (mw-e2e--sans-key url))))
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
                                           (when-let* ((at (mw-e2e--search label line)))
                                             (eq 'bold (get-text-property at 'face line))))
                                         '("Thesaurus" "Dictionary" "Synonym Chooser")))
                    :hidden (save-excursion
                              (goto-char (point-min))
                              (let ((n 0))
                                (while (not (eobp))
                                  (when (invisible-p (point)) (setq n (1+ n)))
                                  (forward-line 1))
                                n))
                    :rule (car (seq-find (lambda (entry)
                                           (and (stringp (car entry))
                                                (string-match-p (car entry) (buffer-name))))
                                         display-buffer-alist))
                    :popup (bound-and-true-p popper-popup-status)
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
          :layout (mapcar (lambda (w) (cons (buffer-name (window-buffer w)) (window-edges w)))
                          (window-list nil 'never))
          :lookups (mw-e2e--texts 'lookup)
          :requests (mw-e2e--texts 'request)
          :fetches (mw-e2e--texts 'fetch)
          :cached (mw-e2e--texts 'cached)
          :warnings (mw-e2e--texts 'warning)
          :plays (mw-e2e--texts 'play)
          :browsed (mw-e2e--texts 'browse)
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

(defvar mw-e2e--own-q nil
  "What q ran in `mw-thesaurus-mode-map' before a case bound its own.")

(defun mw-e2e-own-q (on)
  "Bind q in `mw-thesaurus-mode-map' to `delete-window' when ON, else undo it.
Under evil the key is the remap of `evil-record-macro', as configs do it."
  (let ((key (if (bound-and-true-p evil-mode) [remap evil-record-macro] "q")))
    (if on
        (progn (setq mw-e2e--own-q (lookup-key mw-thesaurus-mode-map key))
               (define-key mw-thesaurus-mode-map key #'delete-window))
      (define-key mw-thesaurus-mode-map key mw-e2e--own-q))))

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
              (at (mw-e2e--search label line))
              (window (get-buffer-window mw-thesaurus-buffer-name)))
    (cons (+ 1 (window-left-column window) at)
          (+ 1 (window-top-line window)))))

(defun mw-e2e--point (buffer)
  "Return point in BUFFER, as its window shows it when it has one."
  (if-let* ((window (get-buffer-window buffer)))
      (window-point window)
    (with-current-buffer buffer (point))))

(defun mw-e2e--search (needle haystack)
  "Return where NEEDLE starts in HAYSTACK, or nil; `string-search' needs Emacs 28."
  (string-match-p (regexp-quote needle) haystack))

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
