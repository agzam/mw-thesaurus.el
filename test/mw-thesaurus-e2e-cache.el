;;; mw-thesaurus-e2e-cache.el --- Keep API answers across E2E Emacsen -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;  MW_E2E_CACHE names a directory: each API answer is fetched once and then
;;  replayed to every version of the package, profile and Emacs, which spares
;;  the shared keys and gives both versions the same answer.
;;
;;; Code:

(require 'url)

(defvar mw-e2e-cache (let ((dir (getenv "MW_E2E_CACHE")))
                       (unless (equal dir "") dir))
  "The directory of saved API answers, or nil to ask the API every time.")

(defvar mw-e2e-cache-log #'ignore
  "A function called with fetch or cached, and the URL without its key.")

(defun mw-e2e--sans-key (url)
  "Return URL without its API key."
  (replace-regexp-in-string "[?&]key=.*" "" url))

(defun mw-e2e--cache-file (url)
  "Return the file that keeps the answer to URL."
  (expand-file-name (concat (secure-hash 'sha1 url) ".http") mw-e2e-cache))

(defun mw-e2e--save-answer (url)
  "Keep the response in the current buffer as the answer to URL, when it is JSON."
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "\r?\n\r?\n" nil t)
               (memq (char-after) '(?\[ ?{)))
      (make-directory mw-e2e-cache t)
      ;; Parallel runs share the directory; a rename never leaves half a file.
      (let* ((file (mw-e2e--cache-file url))
             (tmp (make-temp-file file nil ".tmp"))
             (coding-system-for-write 'binary))
        (write-region (point-min) (point-max) tmp nil 'silent)
        (rename-file tmp file t)))))

(defun mw-e2e--cached-retrieve (orig url callback &optional cbargs &rest args)
  "Replay the kept answer to URL, or fetch it with ORIG and keep it.
CALLBACK, CBARGS and ARGS are those of `url-retrieve'; only API requests
go through the cache, so audio and failing hosts stay live."
  (cond
   ((not (string-match-p "\\`https://www\\.dictionaryapi\\.com/api/" url))
    (apply orig url callback cbargs args))
   ((file-exists-p (mw-e2e--cache-file url))
    (funcall mw-e2e-cache-log 'cached (mw-e2e--sans-key url))
    (let ((buffer (generate-new-buffer " *mw-e2e-cache*")))
      (with-current-buffer buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally (mw-e2e--cache-file url)))
      ;; About the API's latency, so two lookups in a row still overlap.
      (run-at-time 0.3 nil (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (apply callback nil cbargs)))))
      buffer))
   (t
    (funcall mw-e2e-cache-log 'fetch (mw-e2e--sans-key url))
    (apply orig url
           (lambda (status &rest rest)
             (unless (plist-get status :error)
               (mw-e2e--save-answer url))
             (apply callback status rest))
           cbargs args))))

(when mw-e2e-cache
  (advice-add 'url-retrieve :around #'mw-e2e--cached-retrieve '((name . mw-e2e-cache))))

(provide 'mw-thesaurus-e2e-cache)
;;; mw-thesaurus-e2e-cache.el ends here
