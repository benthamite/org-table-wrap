EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  -L . -L test \
	  -l org-table-wrap.el \
	  -l org-table-wrap-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile org-table-wrap.el

clean:
	rm -f *.elc
