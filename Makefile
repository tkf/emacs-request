CARTON ?= carton
EMACS ?= emacs

EL4T_SCRIPT = tools/el4t/emacs.sh
EL4T_CARTON = EL4T_EMACS=${EMACS} EMACS=${EL4T_SCRIPT} ${CARTON}
EL4T_CARTON_EMACS = ${EL4T_CARTON} exec ${EL4T_SCRIPT}

TEST_1 = ${MAKE} EMACS=${EMACS} CARTON=${CARTON} test-1

.PHONY : test test-all test-1 compile clean clean-elpa clean-elc \
	print-deps travis-ci

test: elpa
	${MAKE} test-3

test-3: test-3-tornado test-3-flask

test-3-tornado:
	EL_REQUEST_TEST_SERVER=tornado ${MAKE} test-2

test-3-flask:
	EL_REQUEST_TEST_SERVER=flask ${MAKE} test-2

# Run test for different backends, for one server.
test-2: test-2-url-retrieve test-2-curl

test-2-url-retrieve:
	EL_REQUEST_BACKEND=url-retrieve ${TEST_1}

test-2-curl:
	EL_REQUEST_BACKEND=curl ${TEST_1}

# Run test without checking elpa directory.
test-1:
	${EL4T_CARTON_EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

${EL4T_SCRIPT}:
	git submodule update --init

elpa: ${EL4T_SCRIPT}
	mkdir elpa
	${EL4T_CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

compile: clean-elc elpa
	${EL4T_CARTON_EMACS} -Q -batch -L . -L tests \
		-f batch-byte-compile *.el */*.el

clean-elc:
	rm -f *.elc */*.elc

clean: clean-elpa clean-elc
