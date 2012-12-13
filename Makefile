CARTON ?= carton
EMACS ?= emacs

.PHONY : test test-1 clean-elpa	print-deps travis-ci

test: elpa
	make EMACS=${EMACS} CARTON=${CARTON} test-1

test-1:
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

elpa:
	${CARTON} install

clean-elpa:
	rm -rf elpa

print-deps: elpa
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
