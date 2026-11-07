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

(defconst mw-e2e--dir
  (file-name-as-directory (or (getenv "MW_E2E_DIR") (expand-file-name ".e2e" mw-e2e--root)))
  "Where a run keeps its packages, sockets and profiles, MW_E2E_DIR or .e2e.")

(defconst mw-e2e--profiles
  (split-string (or (getenv "MW_E2E_PROFILES") "vanilla evil") "[ ,]+" t)
  "The profiles of this run; test/mw-thesaurus-e2e-init.el defines them.")

(defconst mw-e2e--evil-profiles '("evil" "doom" "evil-local-q" "younger")
  "The profiles that turn on evil.")

(defconst mw-e2e--window-cases
  '("dwim, word middle" "dictionary dwim" "switch with o" "back across references"
    "drill down with RET" "q quits" "reopen after q" "q on a dictionary page"
    "frame already split" "dictionary, frame already split" "two lookups at once"
    "dwim, word only the dictionary has")
  "The cases of the profiles that only add a window rule.")

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

(defun mw-e2e--prompt-case (command label word expect)
  "Return a case typing WORD at the prompt of COMMAND.
LABEL names it; EXPECT is its plist of checks.  A case that does not
say what the buffer shows passes whatever entries come back."
  (unless (or (plist-get expect :shows) (plist-get expect :no-thesaurus))
    (error "Case %S expects neither :shows nor :no-thesaurus" label))
  (list label :text "\n" :point 1 :tags '(word)
        :steps `((cmd ,command) (wait-prompt) (type ,word) (keys "Enter") (wait-done))
        :expect expect))

(defun mw-e2e--word-case (label word &rest expect)
  "Return a case typing WORD at the prompt of `mw-thesaurus-lookup'.
See `mw-e2e--prompt-case' for LABEL and EXPECT."
  (mw-e2e--prompt-case "mw-thesaurus-lookup" label word expect))

(defun mw-e2e--dictionary-case (label word &rest expect)
  "Return a case typing WORD at the prompt of the dictionary command.
A case that expects a page expects a dictionary page.  See
`mw-e2e--prompt-case' for LABEL and EXPECT."
  (mw-e2e--prompt-case "mw-thesaurus-dictionary-lookup-dwim" label word
                       (if (plist-get expect :shows)
                           (append expect '(:reference dictionary :current "Dictionary"))
                         expect)))

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
     ("C-c C-o opens the website" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "C-c" "C-o") (pause 1))
      :expect (:shows "^\\* happy " :windows 2 :lookups 1
               :browsed "https://www.merriam-webster.com/thesaurus/happy"))
     ("drill down with RET" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6))
      :expect (:shows "^\\* glad " :windows 2 :lookups 2))
     ("drill down with dwim" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* glad " :windows 2))
     ("back after drilling down" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :at "glad" :windows 2 :lookups 2))
     ("back with no earlier lookup" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :error "No earlier lookup" :lookups 1))
     ("dictionary dwim" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary :current "Dictionary"
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :windows 2 :lookups 1
               :hook t))
     ("dictionary dwim, word start" :text ,mw-e2e--happy :point 8 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary))
     ("dictionary dwim, right after the word" :text ,mw-e2e--happy :point 13 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary))
     ("dictionary dwim, whitespace prompts" :text "happy   sad\n" :point 7 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt)
              (type "tenacity") (keys "Enter") (wait-done))
      :expect (:shows "^\\* tenacity " :reference dictionary))
     ("dictionary region, two words" :text "We give up now.\n" :point 4 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f") ("v" "e" "w" "e"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* give up " :reference dictionary))
     ("dictionary region, across a line break" :text "It is under the\nweather now.\n"
      :point 7 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f" "M-f") ("v" "e" "w" "e" "w" "e"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* under the weather " :reference dictionary))
     ("dictionary empty prompt" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (keys "Enter")
              (wait-done 8))
      :expect (:no-thesaurus t :echo "dictionary: Word is required"))
     ("dictionary C-g at the prompt" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (keys "C-g") (pause 2))
      :expect (:no-thesaurus t :lookups 0))
     ("header line on a thesaurus page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference thesaurus :current "Thesaurus"
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'"))
     ("click the other label" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (eval (xterm-mouse-mode 1)) (click "Dictionary") (wait-done 6)
              (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* happy " :reference dictionary :current "Dictionary" :lookups 2))
     ("click the label shown" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (eval (xterm-mouse-mode 1)) (click "Thesaurus") (pause 2)
              (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* happy " :reference thesaurus :lookups 1))
     ("switch with o twice" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (keys "o") (wait-done 6) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference synonym-chooser :current "Synonym Chooser"
               :lookups 2 :requests 2))
     ("switch with o three times" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (keys "o") (wait-done 6) (keys "o") (wait-done 6) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference thesaurus :current "Thesaurus"
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :lookups 3 :requests 3))
     ("click Synonym Chooser on a dictionary page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (eval (xterm-mouse-mode 1)) (click "Synonym Chooser") (wait-done 6)
              (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* happy " :reference synonym-chooser :current "Synonym Chooser"
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'"
               :lines ("mean meeting with unforeseen success")
               :faces (mw-thesaurus-example) :lookups 1 :requests 1))
     ("click Synonym Chooser, then Dictionary" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (eval (xterm-mouse-mode 1)) (click "Synonym Chooser") (wait-done 6)
              (click "Dictionary") (wait-done 6) (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* happy " :reference dictionary :current "Dictionary"
               :lines ("^  hap·py ") :lookups 1 :requests 2))
     ("Synonym Chooser of a word without one" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6)
              (eval (xterm-mouse-mode 1)) (click "Synonym Chooser") (wait-done 6)
              (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* glad$" :reference synonym-chooser :current "Synonym Chooser"
               :lines ("^  Merriam-Webster doesn't explain how the synonyms of \"glad\" differ\\.$")
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'"
               :lookups 2 :requests 3))
     ("o twice before the answer" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (eval (mw-e2e-slow-replies 2))
              (keys "o" "o") (wait-done 15 3))
      :expect (:shows "^\\* happy " :reference dictionary :windows 2 :lookups 3))
     ("dictionary page indents its body" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary :indented t
               :lines ("^  hap·py " "^  1\\.$" "^     a\\. enjoying ")))
     ("org-indent-mode, dictionary page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (add-hook 'org-mode-hook #'org-indent-mode))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary
               :lines ("^hap·py " "^1\\.$" "^   a\\. enjoying ")))
     ("org-indent-mode, back to a dictionary page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (add-hook 'org-mode-hook #'org-indent-mode))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 1
               :lines ("^hap·py " "^1\\.$" "^   a\\. enjoying ")
               :faces (mw-thesaurus-example mw-thesaurus-syllables)))
     ("RET plays a pronunciation" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (setq mw-thesaurus-audio-player '("true")))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "ha-p") (keys "Enter") (wait-for (mw-e2e--texts 'play)))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 1 :played "happy001"))
     ("thesaurus speaker plays" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (setq mw-thesaurus-audio-player '("true")))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (eval (mw-e2e-goto-speaker 1)) (keys "Enter") (wait-for (mw-e2e--texts 'play)))
      :expect (:shows "^\\* happy " :reference thesaurus :lookups 1 :played "happy001"
               :lines ("^\\* happy ~adjective~ \\[\\[mw-thesaurus-entry-audio:[-0-9a-f]+:happy\\]\\[🔊\\]\\]$")))
     ("thesaurus speaker without a recording" :text "\n" :point 1 :tags (flow)
      :steps ((eval (setq mw-thesaurus-audio-player '("true")))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "run") (keys "Enter") (wait-done)
              (eval (mw-e2e-goto-speaker 2)) (keys "Enter") (wait-for (mw-e2e--texts 'outcome)))
      :expect (:shows "^\\* run " :reference thesaurus :lookups 1
               :echo "No Merriam-Webster recording of \"run\""))
     ("RET plays without a player" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (setq mw-thesaurus-audio-player nil exec-path nil))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "ha-p") (keys "Enter") (pause 1))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 1
               :error "No audio player found"))
     ("org-indent-mode, thesaurus page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (add-hook 'org-mode-hook #'org-indent-mode))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference thesaurus :flush t))
     ("org-indent-mode, back across references" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (add-hook 'org-mode-hook #'org-indent-mode))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :reference thesaurus :flush t :lookups 2))
     ("o with no dictionary entry" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "drain away") (keys "Enter")
              (wait-done) (keys "o") (wait-done 6))
      :expect (:shows "^\\* drain (away) " :reference thesaurus
               :echo "No Merriam-Webster dictionary entry for \"drain away\"" :lookups 2))
     ("o with no thesaurus entry" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "etymology")
              (keys "Enter") (wait-done) (keys "o") (wait-done 6) (keys "o") (wait-done 6))
      :expect (:shows "^\\* etymology$" :reference synonym-chooser
               :echo "No Merriam-Webster entry for \"etymology\"" :lookups 2))
     ("o after a misspelling" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "hapy") (keys "Enter")
              (wait-done) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 3))
     ("o lands on a suggestion" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "lisp")
              (keys "Enter") (wait-done) (keys "o") (wait-done 6) (keys "o") (wait-done 6))
      :expect (:shows "^\\* lip " :reference thesaurus :echo "showing \"lip\""))
     ("dwim, word only the dictionary has" :text "It is famously hard.\n" :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done))
      :expect (:shows "^\\* famously " :reference dictionary :current "Dictionary"
               :header "\\` Dictionary   Synonym Chooser\\'" :windows 2 :lookups 1))
     ("dictionary prompt, word only the thesaurus has" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "drain away")
              (keys "Enter") (wait-done))
      :expect (:shows "^\\* drain (away) " :reference thesaurus :current "Thesaurus"
               :header "\\` Thesaurus\\'" :lookups 1))
     ("o on a page only one reference has" :text "It is famously hard.\n" :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (pause 1))
      :expect (:shows "^\\* famously$" :reference synonym-chooser
               :lines ("^  Merriam-Webster doesn't explain how the synonyms of \"famously\"")
               :header "\\` Dictionary   Synonym Chooser\\'" :lookups 1 :requests 2))
     ("RET from a page only one reference has" :text "It is famously hard.\n" :point 9
      :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "celebrated") (keys "Enter") (wait-done 6))
      :expect (:shows "^\\* celebrated " :reference dictionary
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :lookups 2))
     ("back to a page only one reference has" :text "It is famously hard.\n" :point 9
      :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "celebrated") (keys "Enter") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* famously " :reference dictionary :header "\\` Dictionary   Synonym Chooser\\'"
               :lookups 2))
     ("o on a page only the thesaurus has" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "drain away")
              (keys "Enter") (wait-done) (keys "o") (pause 1))
      :expect (:shows "^\\* drain (away) " :reference thesaurus
               :error "No Merriam-Webster dictionary entry for \"drain away\"" :lookups 1))
     ("C-c C-o on a page only one reference has" :text "It is famously hard.\n" :point 9
      :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "C-c" "C-o") (pause 1))
      :expect (:shows "^\\* famously " :reference dictionary :lookups 1
               :browsed "https://www.merriam-webster.com/dictionary/famously"))
     ("click the label of a page only one reference has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (eval (xterm-mouse-mode 1)) (click "Dictionary") (pause 2)
              (eval (xterm-mouse-mode -1)))
      :expect (:shows "^\\* famously " :reference dictionary :header "\\` Dictionary   Synonym Chooser\\'"
               :lookups 1))
     ("back after o on a page only one reference has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (pause 1)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* famously " :reference dictionary
               :header "\\` Dictionary   Synonym Chooser\\'" :lookups 1))
     ("o after going back to a page only one reference has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "celebrated") (keys "Enter") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1) (keys "o") (pause 1))
      :expect (:shows "^\\* famously$" :reference synonym-chooser
               :header "\\` Dictionary   Synonym Chooser\\'" :lookups 2))
     ("o after RET from a page only one reference has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "celebrated") (keys "Enter") (wait-done 6)
              (keys "o") (wait-done 6) (keys "o") (wait-done 6))
      :expect (:shows "^\\* celebrated " :reference thesaurus :current "Thesaurus"
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :lookups 3))
     ("thesaurus dwim on a page only one reference has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "celebrated") (cmd "mw-thesaurus-lookup-dwim") (wait-done 6))
      :expect (:shows "^\\* celebrated " :reference thesaurus
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :lookups 2))
     ("at-point command, word only the dictionary has" :text "It is famously hard.\n"
      :point 9 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-at-point") (wait-done))
      :expect (:shows "^\\* famously " :reference dictionary :header "\\` Dictionary   Synonym Chooser\\'"
               :windows 2 :hook t))
     ("dictionary region, phrase only the thesaurus has" :text "Hope can drain away.\n"
      :point 10 :tags (flow)
      :steps ((region ("C-Space" "M-f" "M-f") ("v" "e" "w" "e"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* drain (away) " :reference thesaurus :header "\\` Thesaurus\\'"))
     ("other reference's answer lands during M-x" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (keys "M-x") (wait-prompt) (wait-done) (type "ignore") (keys "Enter") (pause 1))
      :expect (:shows "^\\* famously " :reference dictionary :lookups 1))
     ("q while a lookup waits for its answer" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (eval (mw-e2e-slow-replies 2))
              (search "glad") (keys "Enter") (keys "q") (pause 4))
      :expect (:no-thesaurus t :lookups 2))
     ("back while a lookup waits for its answer" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (eval (mw-e2e-slow-replies 2))
              (search "fortune") (keys "Enter") (profile-keys ("l") ("H")) (pause 4))
      :expect (:shows "^\\* happy " :reference thesaurus :lookups 3))
     ("back through both references" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (search "fortune") (keys "Enter") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1) (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :reference thesaurus :current "Thesaurus" :lookups 3))
     ("back past the first page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1) (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :reference dictionary :error "No earlier lookup" :lookups 1
               :faces (mw-thesaurus-example mw-thesaurus-syllables)))
     ("RET on the syllables" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "hap·py") (keys "Enter") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 2))
     ("C-c C-o in the dictionary" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "fortune") (keys "C-c" "C-o") (pause 1))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 1
               :browsed "https://www.merriam-webster.com/dictionary/happy"))
     ("C-c C-o on a pronunciation plays it" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (setq mw-thesaurus-audio-player '("true")))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "ha-p") (keys "C-c" "C-o") (wait-for (mw-e2e--texts 'play)))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 1 :played "happy001"))
     ("C-c C-o after a misspelling" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "hapy") (keys "Enter")
              (wait-done) (keys "C-c" "C-o") (pause 1))
      :expect (:shows "^\\* happy " :reference thesaurus
               :browsed "https://www.merriam-webster.com/thesaurus/happy"))
     ("thesaurus dwim on a dictionary page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "fortune") (cmd "mw-thesaurus-lookup-dwim") (wait-done 6))
      :expect (:shows "^\\* fortune " :reference thesaurus :lookups 2))
     ("dictionary dwim on a thesaurus page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done 6))
      :expect (:shows "^\\* glad " :reference dictionary :lookups 2))
     ("q on a dictionary page" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done) (keys "q") (pause 1)
              (keys "C-g") (pause 1))
      :expect (:no-thesaurus t))
     ("dictionary in a read-only buffer" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (with-current-buffer "e2e.txt" (read-only-mode 1)))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary))
     ("dictionary, frame already split" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (let ((other (split-window-right)))
                      (set-window-buffer other (get-buffer-create "*scratch*"))))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary :windows 2))
     ("both references at once" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter")
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "sad")
              (keys "Enter") (wait-done 20 2.5))
      :expect (:windows 2 :lookups 2))
     ("dictionary answer lands during M-x" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((keys "M-x") (wait-prompt)
              (eval (run-at-time 0 nil #'mw-thesaurus--lookup-word "happy" 'dictionary))
              (wait-done) (type "ignore") (keys "Enter") (pause 1))
      :expect (:shows "^\\* happy " :reference dictionary))
     ("writable page types o" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (keys "C-x" "C-q") (profile-keys ("C-e" "o") ("o" "Escape")) (pause 1)
              (keys "C-x" "C-q"))
      :expect (:shows "^\\* happy " :reference thesaurus :lookups 1))
     ("keys come back after C-x C-q twice" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (keys "C-x" "C-q") (keys "C-x" "C-q") (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 2))
     ("user key in the read-only map" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (define-key mw-thesaurus-read-only-map
                      (if (bound-and-true-p evil-mode) [remap evil-delete-char] "x")
                      #'mw-thesaurus--quit))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done) (keys "x") (pause 1)
              (eval (define-key mw-thesaurus-read-only-map
                      (if (bound-and-true-p evil-mode) [remap evil-delete-char] "x")
                      nil)))
      :expect (:no-thesaurus t))
     ("dictionary prompt, typo" :text "\n" :point 1 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt)
              (type "declne") (keys "Enter") (wait-done))
      :expect (:shows "^\\* decline " :reference dictionary :echo "showing \"decline\""))
     ("switch with o" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary :windows 2 :lookups 2))
     ("back across references" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (wait-done 6)
              (profile-keys ("l") ("H")) (pause 1))
      :expect (:shows "^\\* happy " :reference thesaurus :windows 2 :lookups 2))
     ("drill down in the dictionary" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (search "fortune") (keys "Enter") (wait-done 6))
      :expect (:shows "^\\* fortune " :reference dictionary :lookups 2))
     ("writable page types the keys" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6)
              (keys "C-x" "C-q") (profile-keys ("C-e" "l" "Enter") ("H" "Enter")) (pause 1)
              (keys "C-x" "C-q"))
      :expect (:shows "^\\* glad " :windows 2 :lookups 2))
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
      :expect (:shows "^\\* happy "))
     ("q of one's own in the mode map" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (mw-e2e-own-q t)) (cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (keys "q") (pause 1) (eval (mw-e2e-own-q nil)))
      :expect (:shows "^\\* happy " :windows 1))
     ("user key over a default one" :text ,mw-e2e--happy :point 10 :tags (flow)
      :steps ((eval (if (fboundp 'evil-define-key)
                        (evil-define-key 'normal mw-thesaurus-mode-map "x" #'mw-thesaurus--quit)
                      (define-key mw-thesaurus-mode-map (kbd "C-c C-c") #'mw-thesaurus--quit)))
              (cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (profile-keys ("C-c" "C-c") ("x")) (pause 1)
              (eval (if (fboundp 'evil-define-key)
                        (evil-define-key 'normal mw-thesaurus-mode-map "x" nil)
                      (define-key mw-thesaurus-mode-map (kbd "C-c C-c") nil))))
      :expect (:no-thesaurus t)))
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
    (mw-e2e--word-case "word Both" "Both" :shows "^\\* both " :reference 'dictionary
                       :header "\\` Dictionary   Synonym Chooser\\'")
    (mw-e2e--word-case "word the" "the" :shows "^\\* the " :reference 'dictionary)
    (mw-e2e--word-case "word and" "and" :shows "^\\* and " :reference 'dictionary)
    (mw-e2e--word-case "word neither" "neither" :shows "^\\* neither " :only "neither")
    (mw-e2e--word-case "word is" "is" :shows "^\\* is " :only "\\<\\(is\\|be\\)\\>")
    (mw-e2e--word-case "typo hapy" "hapy" :shows "^\\* happy " :echo "showing \"happy\"")
    (mw-e2e--word-case "typo HAPY" "HAPY" :shows "^\\* happy ")
    (mw-e2e--word-case "typo recieve" "recieve" :shows "^\\* receive ")
    (mw-e2e--word-case "typo definately" "definately"
                       :shows "^\\* definitely " :only "\\<definite")
    (mw-e2e--word-case "typo seperate" "seperate"
                       :shows "^\\* separate " :only "separate" :echo "showing \"separate\"")
    (mw-e2e--word-case "missing etymology" "etymology" :shows "^\\* etymology "
                       :reference 'dictionary)
    (mw-e2e--word-case "missing lisp" "lisp" :shows "^\\* lip " :echo "showing \"lip\"")
    (mw-e2e--word-case "closest word notoriously" "notoriously" :shows "^\\* notorious "
                       :echo "showing \"notorious\"")
    (mw-e2e--word-case "missing famously" "famously" :shows "^\\* famously "
                       :reference 'dictionary :header "\\` Dictionary   Synonym Chooser\\'" :lookups 1)
    (mw-e2e--word-case "missing Famously" "Famously" :shows "^\\* famously "
                       :reference 'dictionary)
    (mw-e2e--word-case "missing FAMOUSLY" "FAMOUSLY" :shows "^\\* famously "
                       :reference 'dictionary)
    (mw-e2e--word-case "missing kubernetes" "kubernetes" :no-thesaurus t :lookups 1
                       :echo "No Merriam-Webster entry for \"kubernetes\"")
    (mw-e2e--word-case "gibberish" "xqzvbnwpt" :no-thesaurus t :lookups 1)
    (mw-e2e--word-case "apostrophe don't" "don't" :shows "^\\* don " :echo "showing \"don\"")
    (mw-e2e--word-case "apostrophe can't" "can't" :shows "^\\* can't " :reference 'dictionary)
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
    (mw-e2e--word-case "single letter a" "a" :shows "^\\* a " :reference 'dictionary)
    (mw-e2e--word-case "single letter I" "I" :shows "^\\* i " :reference 'dictionary :lookups 1)
    (mw-e2e--word-case "number" "123" :no-thesaurus t)
    (mw-e2e--dictionary-case "dictionary happy" "happy" :shows "^\\* happy " :only "happy")
    (mw-e2e--dictionary-case "dictionary Happy" "Happy" :shows "^\\* happy " :only "happy")
    (mw-e2e--dictionary-case "dictionary HAPPY" "HAPPY" :shows "^\\* happy " :only "happy")
    (mw-e2e--dictionary-case "dictionary weakest" "weakest" :shows "^\\* weak " :only "weak")
    (mw-e2e--dictionary-case "dictionary running" "running" :shows "^\\* running "
                             :only "\\<\\(running\\|run\\|ran\\)\\>")
    (mw-e2e--dictionary-case "dictionary went" "went" :shows "^\\* went$"
                             :only "\\<\\(went\\|go\\)\\>")
    (mw-e2e--dictionary-case "dictionary mice" "mice" :shows "^\\* mice$"
                             :only "\\<\\(mice\\|mouse\\)\\>")
    (mw-e2e--dictionary-case "dictionary tenacity" "tenacity" :shows "^\\* tenacity "
                             :only "tenacity")
    (mw-e2e--dictionary-case "dictionary the" "the" :shows "^\\* the ")
    (mw-e2e--dictionary-case "dictionary a" "a" :shows "^\\* a ")
    (mw-e2e--dictionary-case "dictionary I" "I" :shows "^\\* i ")
    (mw-e2e--dictionary-case "dictionary and" "and" :shows "^\\* and ")
    (mw-e2e--dictionary-case "dictionary Both" "Both" :shows "^\\* both ")
    (mw-e2e--dictionary-case "dictionary lisp" "lisp" :shows "^\\* lisp " :lookups 1)
    (mw-e2e--dictionary-case "dictionary etymology" "etymology" :shows "^\\* etymology "
                             :only "etymology")
    (mw-e2e--dictionary-case "dictionary typo hapy" "hapy" :shows "^\\* happy "
                             :echo "dictionary entry for \"hapy\", showing \"happy\"")
    (mw-e2e--dictionary-case "dictionary typo recieve" "recieve" :shows "^\\* receive ")
    (mw-e2e--dictionary-case "dictionary typo definately" "definately"
                             :shows "^\\* definitely ")
    (mw-e2e--dictionary-case "dictionary typo seperate" "seperate" :shows "^\\* separate ")
    (mw-e2e--dictionary-case "dictionary typo declne" "declne" :shows "^\\* decline ")
    (mw-e2e--dictionary-case "dictionary faces decline" "decline" :shows "^\\* decline "
                             :lines '("^  de·cline \\[\\[mw-thesaurus-audio:declin03\\]\\[di-ˈklīn\\]\\] dē-$"
                                      "^   5\\.$" "^      a\\. to slope downward")
                             :faces '(mw-thesaurus-example mw-thesaurus-pronunciation
                                      mw-thesaurus-syllables))
    (mw-e2e--dictionary-case "dictionary gibberish" "xqzvbnwpt" :no-thesaurus t :lookups 1
                             :echo "No Merriam-Webster dictionary entry for \"xqzvbnwpt\"")
    (mw-e2e--dictionary-case "dictionary apostrophe don't" "don't" :shows "^\\* don't ")
    (mw-e2e--dictionary-case "dictionary apostrophe can't" "can't" :shows "^\\* can't ")
    (mw-e2e--dictionary-case "dictionary hyphen well-known" "well-known"
                             :shows "^\\* well-known " :only "well-known")
    (mw-e2e--dictionary-case "dictionary non-ASCII café" "café" :shows "^\\* café ")
    (mw-e2e--dictionary-case "dictionary non-ASCII naïve" "naïve" :shows "^\\* naive ")
    (mw-e2e--dictionary-case "dictionary non-ASCII résumé" "résumé" :shows "^\\* resume ")
    (mw-e2e--dictionary-case "dictionary spaces around" "  happy  " :shows "^\\* happy ")
    (mw-e2e--dictionary-case "dictionary punctuation happy!" "happy!" :shows "^\\* happy ")
    (mw-e2e--dictionary-case "dictionary question what?" "what?" :shows "^\\* what ")
    (mw-e2e--dictionary-case "dictionary slash and/or" "and/or" :shows "^\\* and/or ")
    (mw-e2e--dictionary-case "dictionary syllables hap·py" "hap·py" :shows "^\\* happy ")
    (mw-e2e--dictionary-case "dictionary phrase give up" "give up" :shows "^\\* give up "
                             :only "give up")
    (mw-e2e--dictionary-case "dictionary phrase under the weather" "under the weather"
                             :shows "^\\* under the weather ")
    (mw-e2e--dictionary-case "dictionary phrase giv up" "giv up" :shows "^\\* give up "
                             :echo "showing \"give up\"")
    (mw-e2e--dictionary-case "dictionary phrase break the law" "break the law"
                             :shows "^\\* break/violate the law ")
    (mw-e2e--dictionary-case "dictionary abbreviation etc." "etc." :shows "^\\* etc\\. ")
    (mw-e2e--dictionary-case "dictionary capitals NASA" "NASA" :shows "^\\* NASA ")
    (mw-e2e--dictionary-case "dictionary names Paris" "Paris" :shows "^\\* Paris ")
    (mw-e2e--dictionary-case "dictionary homographs bass" "bass" :shows "^\\* bass ")
    (mw-e2e--dictionary-case "dictionary large entry set" "set" :shows "^\\* set ")
    (mw-e2e--dictionary-case "dictionary biographical name cowl" "cowl" :shows "^\\* cowl ")
    (mw-e2e--dictionary-case "dictionary capital headword borstal" "borstal"
                             :shows "^\\* Borstal ")
    (mw-e2e--dictionary-case "dictionary sentence" "the quick brown fox jumps over the lazy dog"
                             :no-thesaurus t)
    (mw-e2e--dictionary-case "dictionary number" "123" :no-thesaurus t :lookups 1)
    (mw-e2e--dictionary-case "dictionary emoji" "😀" :no-thesaurus t :lookups 1)
    (mw-e2e--dictionary-case "dictionary spaces only" "   " :no-thesaurus t :lookups 1))
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
     ("dictionary bad key" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key "bad-e2e-key"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done))
      :expect (:no-thesaurus t
               :echo "dictionary: Invalid API key.*Set `mw-thesaurus-dictionary-api-key'"))
     ("dictionary without a key" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key nil))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done 8))
      :expect (:no-thesaurus t :error "Set `mw-thesaurus-dictionary-api-key'" :lookups 1))
     ("dictionary unknown host" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus--dictionary-api-url "https://mw-e2e.invalid/collegiate/"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :error "."))
     ("dictionary refused connection" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus--dictionary-api-url "https://127.0.0.1:9/collegiate/"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :echo "dictionary request failed"))
     ("dictionary HTTP 404" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus--dictionary-api-url
                          "https://www.dictionaryapi.com/api/v3/references/collegiate/jsonx/"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done 20))
      :expect (:no-thesaurus t :echo "dictionary request failed"))
     ("word only the dictionary has, without a dictionary key" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key nil))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (wait-done))
      :expect (:no-thesaurus t :echo "No Merriam-Webster entry for \"famously\"" :lookups 1))
     ("word only the dictionary has, with an empty dictionary key" :text "\n" :point 1
      :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key ""))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (wait-done))
      :expect (:no-thesaurus t :echo "No Merriam-Webster entry for \"famously\"" :lookups 1))
     ("dictionary, phrase only the thesaurus has, without a thesaurus key" :text "\n" :point 1
      :tags (env)
      :steps ((eval (setq mw-thesaurus-api-key ""))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "drain away")
              (keys "Enter") (wait-done))
      :expect (:no-thesaurus t :lookups 1
               :echo "No Merriam-Webster dictionary entry for \"drain away\""))
     ("a lookup right after one that falls back" :text "\n" :point 1 :tags (env)
      :steps ((eval (mw-e2e-slow-replies 1))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter")
              (wait-done 20 3))
      :expect (:shows "^\\* happy " :windows 2 :lookups 2))
     ("a lookup right after a typo" :text "\n" :point 1 :tags (env)
      :steps ((eval (mw-e2e-slow-replies 1))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "hapy") (keys "Enter")
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "sad") (keys "Enter")
              (wait-done 20 3))
      :expect (:shows "^\\* sad " :windows 2))
     ("typo, dictionary key refused" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key "bad-e2e-key"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "hapy") (keys "Enter")
              (wait-done))
      :expect (:shows "^\\* happy " :echo "showing \"happy\""))
     ("word only the dictionary has, dictionary key refused" :text "\n" :point 1
      :tags (env)
      :steps ((eval (setq mw-thesaurus-dictionary-api-key "bad-e2e-key"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (wait-done))
      :expect (:no-thesaurus t :echo "dictionary: Invalid API key" :lookups 1))
     ("word only the dictionary has, dictionary HTTP 404" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus--dictionary-api-url
                          "https://www.dictionaryapi.com/api/v3/references/collegiate/jsonx/"))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "famously") (keys "Enter")
              (wait-done 20))
      :expect (:no-thesaurus t :echo "dictionary request failed" :lookups 1))
     ("dictionary, phrase only the thesaurus has, thesaurus key refused" :text "\n" :point 1
      :tags (env)
      :steps ((eval (setq mw-thesaurus-api-key "bad-e2e-key"))
              (cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "drain away")
              (keys "Enter") (wait-done))
      :expect (:no-thesaurus t :echo "\\`Merriam-Webster: Invalid API key" :lookups 1))
     ("recording missing on the server" :text "\n" :point 1 :tags (env)
      :steps ((eval (setq mw-thesaurus-audio-player '("true")))
              (eval (mw-thesaurus--play "nosuch99"))
              (wait-for (mw-e2e--texts 'outcome) 20))
      :expect (:no-thesaurus t :echo "audio failed" :lookups 0))
     ("writable page types q" :text ,mw-e2e--happy :point 10 :tags (env)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-done)
              (keys "C-x" "C-q" "C-e" "q") (pause 1) (keys "C-x" "C-q"))
      :expect (:shows "^\\* happy .*q$" :reference dictionary :lookups 1))
     ;; The upgrade cases go last: they load the checkout into origin/master's Emacs.
     ("upgrade with pages in the history" :text ,mw-e2e--happy :point 10 :tags (env upgrade)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done)
              (search "glad") (keys "Enter") (wait-done 6)
              (eval (let ((inhibit-message t))
                      (load (expand-file-name "mw-thesaurus.el" ,mw-e2e--root) nil t)
                      (setq mw-e2e-defaults (mw-e2e-defaults))))
              (keys "l") (pause 1) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary
               :header "\\` Thesaurus   Dictionary   Synonym Chooser\\'" :lookups 3))
     ("upgrade in a running session" :text "\n" :point 1 :tags (env upgrade)
      :steps ((eval (let ((inhibit-message t))
                      (load (expand-file-name "mw-thesaurus.el" ,mw-e2e--root) nil t)
                      (setq mw-e2e-defaults (mw-e2e-defaults))))
              (cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter") (wait-done))
      :expect (:shows "^\\* happy "))
     ("upgrade, then the dictionary" :text "\n" :point 1 :tags (env upgrade)
      :steps ((cmd "mw-thesaurus-dictionary-lookup-dwim") (wait-prompt) (type "happy")
              (keys "Enter") (wait-done))
      :expect (:shows "^\\* happy " :reference dictionary :current "Dictionary"))
     ("upgrade, then o" :text "\n" :point 1 :tags (env upgrade)
      :steps ((cmd "mw-thesaurus-lookup") (wait-prompt) (type "happy") (keys "Enter")
              (wait-done) (keys "o") (wait-done 6))
      :expect (:shows "^\\* happy " :reference dictionary :lookups 2))
     ("upgrade, then a word only the dictionary has" :text "It is famously hard.\n" :point 9
      :tags (env upgrade)
      :steps ((cmd "mw-thesaurus-lookup-dwim") (wait-done) (keys "o") (pause 1))
      :expect (:shows "^\\* famously$" :reference synonym-chooser
               :header "\\` Dictionary   Synonym Chooser\\'" :lookups 1))))
  "The cases: (LABEL :text :point :tags :steps :expect).
Steps are (cmd NAME), (keys KEY...) in tmux names, (type TEXT),
\(region or profile-keys VANILLA-KEYS EVIL-KEYS), (search TEXT),
\(click LABEL) in the header line, (eval FORM), (wait-prompt),
\(wait-done [SECONDS QUIET]), (wait-for FORM [SECONDS]), (pause SECONDS).")

(defun mw-e2e--evil-p (profile)
  "Non-nil when PROFILE turns on evil."
  (member profile mw-e2e--evil-profiles))

(defun mw-e2e--selected-cases (profile)
  "Return the cases PROFILE runs.
Vanilla runs all, a profile that only adds a window rule a few, and the
others the flow cases."
  (seq-filter (lambda (case)
                (and (or (null mw-e2e--only) (string-match-p mw-e2e--only (car case)))
                     (cond ((equal profile "vanilla"))
                           ((member profile '("rule-literal" "rule-prefix" "popper"))
                            (member (car case) mw-e2e--window-cases))
                           (t (memq 'flow (plist-get (cdr case) :tags))))))
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
  "Install what origin/master and the profiles need into the run's ELPA."
  (setq package-user-dir (expand-file-name "elpa" mw-e2e--dir))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (when-let* ((missing (seq-remove #'package-installed-p '(request dash evil evil-org popper))))
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
           (concat "LANG=" (or (getenv "MW_E2E_LANG") "en_US.UTF-8"))
           "TERM=screen-256color"
           (concat "MW_E2E_ELPA=" (expand-file-name "elpa" mw-e2e--dir))
           (concat "MW_E2E_SOURCE=" source)
           (concat "MW_E2E_PROFILE=" profile)
           (concat "MW_E2E_SOCKET_DIR=" sockets)
           (concat "MW_E2E_SERVER=" name)
           (concat "MW_E2E_DIR=" mw-e2e--dir)
           (concat "MW_E2E_CACHE=" (or (getenv "MW_E2E_CACHE") ""))
           (concat "MW_E2E_COMPILE=" (or (getenv "MW_E2E_COMPILE") ""))
           `(,(expand-file-name invocation-name invocation-directory)
             "-nw" "-q"
             ;; HOME is the run's own, so Emacs 27 and 28 stay sandboxed without it.
             ,@(when (<= 29 emacs-major-version) (list "--init-directory" init-dir))
             "-l" ,(expand-file-name "mw-thesaurus-e2e-init.el" mw-e2e--test-dir)))
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
    (`(,(or 'region 'profile-keys) ,vanilla ,evil)
     (apply #'mw-e2e--keys name (if (mw-e2e--evil-p profile) evil vanilla)) nil)
    (`(search ,text)
     (if (mw-e2e--evil-p profile)
         (progn (mw-e2e--keys name "/") (mw-e2e--type name text) (mw-e2e--keys name "Enter"))
       (mw-e2e--keys name "C-s") (mw-e2e--type name text) (mw-e2e--keys name "Enter" "M-b"))
     (sleep-for 0.3) nil)
    ;; The value may be a window or a buffer, which the server cannot print readably.
    (`(eval ,form) (mw-e2e--eval name `(progn ,form t)) nil)
    ;; An xterm SGR press and release, which `xterm-mouse-mode' reads as mouse-1.
    (`(click ,label)
     (if-let* ((at (mw-e2e--eval name `(mw-e2e-header-position ,label))))
         (progn (mw-e2e--type name (format "\e[<0;%d;%dM\e[<0;%d;%dm"
                                           (car at) (cdr at) (car at) (cdr at)))
                nil)
       (format "no %s in the header line" label)))
    ('(wait-prompt) (mw-e2e--poll name '(and (active-minibuffer-window) t) 5))
    (`(wait-for ,form . ,args) (mw-e2e--poll name form (or (car args) 15)))
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

(defun mw-e2e--save-screen (name label)
  "Keep the screen of the Emacs NAME after the case LABEL under screens/ of the run."
  (let ((file (expand-file-name
               (format "screens/%s/%s.txt" name
                       (replace-regexp-in-string "[^[:alnum:]]+" "-" label))
               mw-e2e--dir))
        (coding-system-for-write 'utf-8))
    (make-directory (file-name-directory file) t)
    (write-region (or (ignore-errors
                        (mw-e2e--tmux "capture-pane" "-p" "-t" (concat "=" name ":")))
                      "")
                  nil file nil 'silent)))

(defun mw-e2e--missing-command (name case)
  "Return the first command CASE types that the Emacs NAME lacks, or nil."
  (seq-some (lambda (step)
              (pcase step
                (`(cmd ,command)
                 (unless (mw-e2e--eval name `(fboundp ',(intern command)))
                   command))))
            (plist-get (cdr case) :steps)))

(defun mw-e2e--run-case (name profile case)
  "Run CASE in the Emacs NAME of PROFILE; return (SECONDS NOTES STATE).
A case typing a command the package lacks fails at once, not after a timeout."
  (if-let* ((missing (mw-e2e--missing-command name case)))
      (list 0 (list (format "no command %s" missing)) nil)
    (mw-e2e--run-case-1 name profile case)))

(defun mw-e2e--run-case-1 (name profile case)
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
      (mw-e2e--save-screen name (car case))
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
                       ;; Only other tabs are named, so thesaurus pages read
                       ;; the same in versions that have no references.
                       (format "shows %S (%d entries%s)" (plist-get thesaurus :head)
                               (plist-get thesaurus :entries)
                               (if (memq (plist-get thesaurus :reference) '(nil thesaurus))
                                   ""
                                 (format ", %s" (plist-get thesaurus :reference))))
                     "no thesaurus")
                   (when-let* ((outcomes (plist-get state :outcomes)))
                     (format "echo %S" (string-join outcomes " | ")))
                   (when-let* ((errors (plist-get state :errors)))
                     (format "error %S" (string-join errors " | ")))
                   (when-let* ((warnings (plist-get state :warnings)))
                     (format "warning %S" (string-join warnings " | ")))
                   (when-let* ((n (and thesaurus (plist-get thesaurus :hidden)))
                               ((< 0 n)))
                     (format "%d lines hidden" n))
                   (when-let* ((rule (and thesaurus (plist-get thesaurus :rule))))
                     (format "rule %S" rule))
                   (when-let* ((popup (and thesaurus (plist-get thesaurus :popup))))
                     (format "popup %s" popup))
                   (format "lookups %S" (plist-get state :lookups))
                   (format "windows %s; selected %s"
                           (mapconcat (lambda (w) (format "%s %S" (car w) (cdr w)))
                                      (plist-get state :layout) ", ")
                           (plist-get state :selected))))
       "; "))))

(defun mw-e2e--normalize (text)
  "Return TEXT without what every branch page adds to an origin/master one.
That is the buffer's new name and the speaker after each thesaurus heading."
  (replace-regexp-in-string
   "\\* Merriam-Webster \\*" "* Merriam-Webster Thesaurus *"
   (replace-regexp-in-string " \\[\\[mw-thesaurus-entry-audio:[^]]*\\]\\[🔊\\]\\]" "" text t t)
   t t))

(defun mw-e2e--page-diff (main branch)
  "Return how the thesaurus page in BRANCH differs from the one in MAIN, or nil.
MAIN and BRANCH are states; only thesaurus pages with the same head line
compare."
  (let ((a (plist-get main :thesaurus))
        (b (plist-get branch :thesaurus)))
    (when (and a b
               (memq (plist-get b :reference) '(nil thesaurus))
               (eq (plist-get a :reference) (plist-get b :reference))
               (equal (mw-e2e--normalize (plist-get a :head))
                      (mw-e2e--normalize (plist-get b :head))))
      (let ((old (mw-e2e--normalize (plist-get a :text)))
            (new (mw-e2e--normalize (plist-get b :text))))
        (unless (equal old new)
          (let ((old-file (make-temp-file "mw-e2e-main"))
                (new-file (make-temp-file "mw-e2e-branch"))
                (coding-system-for-write 'utf-8)
                (coding-system-for-read 'utf-8))
            (unwind-protect
                (progn
                  (write-region old nil old-file nil 'silent)
                  (write-region new nil new-file nil 'silent)
                  (with-temp-buffer
                    (call-process "diff" nil t nil "-u" "--label" "main" "--label" "branch"
                                  old-file new-file)
                    (buffer-string)))
              (delete-file old-file)
              (delete-file new-file))))))))

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
        (unless (plist-get thesaurus :org) (fail "thesaurus not in org-mode"))
        (unless (plist-get thesaurus :mw) (fail "mw-thesaurus-mode off"))
        (unless (plist-get thesaurus :read-only) (fail "thesaurus buffer writable")))
      (when (plist-get state :minibuffer) (fail "a prompt is left open"))
      (when (and (plist-get state :errors) (not (plist-get expect :error)))
        (fail "unexpected error: %s" (string-join (plist-get state :errors) " | ")))
      (when-let* ((warnings (plist-get state :warnings)))
        (fail "warning: %s" (string-join warnings " | ")))
      (when-let* ((re (plist-get expect :error)))
        (unless (seq-some (lambda (e) (string-match-p re e)) (plist-get state :errors))
          (fail "no error matching %S" re)))
      (when-let* ((re (plist-get expect :shows)))
        (unless (and thesaurus (string-match-p re (plist-get thesaurus :head)))
          (fail "expected the thesaurus to show %S" re)))
      (when-let* ((reference (plist-get expect :reference)))
        (unless (and thesaurus (eq reference (plist-get thesaurus :reference)))
          (fail "expected the %s" reference)))
      (when-let* ((re (plist-get expect :header)))
        (unless (and thesaurus (string-match-p re (or (plist-get thesaurus :header) "")))
          (fail "expected a header line matching %S" re)))
      (when-let* ((label (plist-get expect :current)))
        (unless (and thesaurus (equal label (plist-get thesaurus :current)))
          (fail "expected %s in bold in the header line" label)))
      (when (and (plist-get expect :flush)
                 (not (and thesaurus (eql 0 (plist-get thesaurus :indented)))))
        (fail "expected no line to start with a space"))
      (when (and (plist-get expect :indented)
                 (not (and thesaurus (< 0 (or (plist-get thesaurus :indented) 0)))))
        (fail "expected indented body lines"))
      (when-let* ((url (plist-get expect :browsed)))
        (unless (equal (list url) (plist-get state :browsed))
          (fail "expected the browser to open %s, not %S" url (plist-get state :browsed))))
      (dolist (re (plist-get expect :lines))
        (unless (and thesaurus (string-match-p re (plist-get thesaurus :text)))
          (fail "no line matching %S" re)))
      (dolist (face (plist-get expect :faces))
        (unless (and thesaurus (memq face (plist-get thesaurus :faces)))
          (fail "no text in %s" face)))
      (when-let* ((audio (plist-get expect :played)))
        (let ((file (concat audio ".mp3")))
          (unless (seq-some (lambda (play) (string-suffix-p (concat "/" file) play))
                            (plist-get state :plays))
            (fail "%s did not play" audio))
          ;; A real recording, not an error page.
          (unless (< 1000 (or (cdr (assoc file (plist-get state :audio))) 0))
            (fail "%s did not download" audio))))
      (when-let* ((re (plist-get expect :at)))
        (unless (and thesaurus
                     (string-match-p (concat "\\`" re) (plist-get thesaurus :after-point)))
          (fail "expected point in the thesaurus before %S" re)))
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
      (when-let* ((n (plist-get expect :requests)))
        (unless (= n (length (plist-get state :requests)))
          (fail "expected %d API requests, not %S" n (plist-get state :requests))))
      (when (and (plist-get expect :hook) (not (plist-get state :hook)))
        (fail "mw-thesaurus-mode-hook did not run")))
    (nreverse failures)))

;;; Runs

(defvar mw-e2e--totals (list :cases 0 :failures 0 :same 0 :fixed 0 :diffs 0 :texts 0
                              :requests 0 :fetches 0 :cached 0)
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
          (let (outcomes failures states)
            (cl-loop
             for name in names for flavor in mw-e2e--flavors
             do (pcase-let* ((`(,seconds ,notes ,state) (mw-e2e--run-case name profile case))
                             (outcome (mw-e2e--outcome state)))
                  (push outcome outcomes)
                  (push state states)
                  (push (append notes (mw-e2e--failures state (plist-get (cdr case) :expect)))
                        failures)
                  (mw-e2e--count :requests (length (plist-get state :lookups)))
                  (mw-e2e--count :fetches (length (plist-get state :fetches)))
                  (mw-e2e--count :cached (length (plist-get state :cached)))
                  (princ (format "%-12s %-6s %-30s %2ss => %s\n" profile flavor (car case)
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
            (unless (equal (mw-e2e--normalize (car outcomes))
                           (mw-e2e--normalize (cadr outcomes)))
              (mw-e2e--count :diffs)
              (princ (format "  DIFF %s\n" (car case))))
            (when-let* ((diff (mw-e2e--page-diff (cadr states) (car states))))
              (mw-e2e--count :texts)
              (princ (format "  TEXT %s\n" (car case)))
              (dolist (line (seq-take (split-string diff "\n" t) 60))
                (princ (format "    %s\n" line))))))
      (cl-loop for name in names
               do (princ (format "%s asks before exit about: %S\n" name
                                 (ignore-errors (mw-e2e--eval name '(mw-e2e-processes))))))
      (mapc #'mw-e2e--stop names))))

(defun mw-e2e-main ()
  "Run every profile of `mw-e2e--profiles' and exit non-zero on a failure."
  (unless (seq-some #'mw-e2e--selected-cases mw-e2e--profiles)
    (error "No case label matches MW_E2E_CASES %S, an Emacs regexp (\\| between labels)"
           mw-e2e--only))
  (make-directory mw-e2e--dir t)
  (mw-e2e--packages)
  (unwind-protect
      (dolist (profile mw-e2e--profiles)
        (mw-e2e--profile profile))
    (ignore-errors (mw-e2e--tmux "kill-server")))
  (princ (format (concat "SUMMARY %d cases: %d FAIL, %d SAME, %d FIXED, %d DIFF, %d TEXT; "
                         "%d lookups, %d API requests, %d cached answers\n")
                 (plist-get mw-e2e--totals :cases) (plist-get mw-e2e--totals :failures)
                 (plist-get mw-e2e--totals :same) (plist-get mw-e2e--totals :fixed)
                 (plist-get mw-e2e--totals :diffs) (plist-get mw-e2e--totals :texts)
                 (plist-get mw-e2e--totals :requests) (plist-get mw-e2e--totals :fetches)
                 (plist-get mw-e2e--totals :cached)))
  (kill-emacs (if (zerop (plist-get mw-e2e--totals :failures)) 0 1)))

(provide 'mw-thesaurus-e2e)
;;; mw-thesaurus-e2e.el ends here
