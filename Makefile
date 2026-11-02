.PHONY: help deps test lint checkdoc check-compile clean

ELPA := $(CURDIR)/.elpa
SANDBOX := $(CURDIR)/.sandbox

# Every Emacs here runs against a throwaway user-emacs-directory: -Q
# and --batch skip init files but still write under ~/.emacs.d.  The
# first --eval moves it, because --init-directory arrived in Emacs 29
# and Emacs 27.1 is the oldest version this package supports.
EMACS_Q = emacs -Q \
	--eval "(setq user-emacs-directory \"$(SANDBOX)/\")" \
	--eval "(when (and (featurep 'native-compile) (fboundp 'startup-redirect-eln-cache)) (startup-redirect-eln-cache \"$(SANDBOX)/eln-cache/\"))" \
	--eval "(setq package-user-dir \"$(ELPA)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))"

EMACS = $(EMACS_Q) --batch

help:
	@echo "Available commands:"
	@echo "  make deps          Install package-lint into .elpa"
	@echo "  make test          Run the tests"
	@echo "  make lint          Run package-lint"
	@echo "  make checkdoc      Run checkdoc"
	@echo "  make check-compile Check for clean byte-compilation"
	@echo "  make clean         Remove compiled files and the sandbox"

deps:
	@echo "Installing dependencies into $(ELPA)"
	$(EMACS) --eval "(progn (package-initialize) (package-refresh-contents) \
	  (unless (package-installed-p 'package-lint) (package-install 'package-lint)))"

$(ELPA):
	@$(MAKE) deps

test:
	$(EMACS) -L . -l mw-thesaurus-test.el -f ert-run-tests-batch-and-exit

lint: | $(ELPA)
	$(EMACS) -f package-initialize -L . \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit mw-thesaurus.el

# the experimental verb check is on in some Emacs versions and off in
# others, and MELPA's reviewers run with it off
checkdoc:
	@out=$$($(EMACS) \
	  --eval "(require 'checkdoc)" \
	  --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
	  --eval "(checkdoc-file \"mw-thesaurus.el\")" 2>&1); \
	if [ -n "$$out" ]; then echo "$$out"; exit 1; else echo "checkdoc: clean"; fi

# the .elc goes at the end: `locate-library' prefers it, so one left
# behind makes a later `make test' run against a stale artifact
check-compile:
	@echo "Checking byte-compilation..."
	@$(EMACS) -L . \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(unless (byte-compile-file \"mw-thesaurus.el\") (kill-emacs 1))" \
	  || { rm -f *.elc; exit 1; }
	@rm -f *.elc

clean:
	rm -f *.elc
	rm -rf $(ELPA) $(SANDBOX)
