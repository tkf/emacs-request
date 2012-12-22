CARTON ?= carton
EMACS ?= emacs

EL4T_SCRIPT = tools/el4t/emacs.sh
EL4T_CARTON = EL4T_EMACS=${EMACS} EMACS=${EL4T_SCRIPT} ${CARTON}
EL4T_CARTON_EMACS = ${EL4T_CARTON} exec ${EL4T_SCRIPT}

TEST_1 = make EMACS=${EMACS} CARTON=${CARTON} test-1

.PHONY : test test-1 compile clean-elpa clean-elc print-deps travis-ci

test: elpa
	EL_REQUEST_BACKEND=url-retrieve ${TEST_1}
	EL_REQUEST_BACKEND=curl ${TEST_1}

test-1:
	${EL4T_CARTON_EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

elpa:
	${EL4T_CARTON} install

clean-elpa:
	rm -rf elpa

compile: clean-elc
	${EL4T_CARTON_EMACS} -Q -batch -L . -L tests \
		-f batch-byte-compile *.el */*.el

clean-elc:
	rm -f *.elc */*.elc

print-deps: elpa
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	curl --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
