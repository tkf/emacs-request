CARTON ?= carton
EMACS ?= emacs

EL4T_SCRIPT = tools/el4t/emacs.sh
EL4T_CARTON = EL4T_EMACS=${EMACS} EMACS=${EL4T_SCRIPT} ${CARTON}
EL4T_CARTON_EMACS = ${EL4T_CARTON} exec ${EL4T_SCRIPT}

TEST_1 = make EMACS=${EMACS} CARTON=${CARTON} test-1

.PHONY : test test-all test-1 compile clean clean-elpa clean-elc \
	print-deps travis-ci

test: elpa
	EL_REQUEST_TEST_SERVER=tornado make test-2
	EL_REQUEST_TEST_SERVER=flask   make test-2

# Run test for different backends, for one server.
test-2:
	EL_REQUEST_BACKEND=url-retrieve ${TEST_1}
	EL_REQUEST_BACKEND=curl ${TEST_1}

# Run test without checking elpa directory.
test-1:
	${EL4T_CARTON_EMACS} -Q -batch \
		-L . -L tests -l tests/test-request.el \
		-f ert-run-tests-batch-and-exit

elpa:
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

print-deps: elpa
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	curl --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps test



# Run test against Emacs listed in ${EL4T_EMACS_LIST}.
# This is for running tests for multiple Emacs versions.
# This is not used in Travis CI.  Usage::
#
#     make EL4T_EMACS_LIST="emacs emacs-snapshot emacs23" test-all
#
# See: http://stackoverflow.com/a/12110773/727827

JOBS := $(addprefix job-,${EL4T_EMACS_LIST})
.PHONY: ${JOBS}

${JOBS}: job-%:
	make EMACS=$* clean test

test-all: ${JOBS}
