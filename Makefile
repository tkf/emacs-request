CARTON ?= carton
EMACS ?= emacs
TEST_1 = make EMACS=${EMACS} CARTON=${CARTON} test-1

.PHONY : test test-1 clean-elpa clean print-deps travis-ci

test: elpa
	EL_REQUEST_BACKEND=url-retrieve ${TEST_1}
	EL_REQUEST_BACKEND=curl ${TEST_1}

test-1:
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

elpa:
	${CARTON} install

clean:
	rm -f *.elc */*.elc

clean-elpa:
	rm -rf elpa

print-deps: elpa
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	curl --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
