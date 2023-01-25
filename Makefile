export CASK ?= cask
export EMACS ?= $(shell which emacs)
export CASK_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

PKBUILD=2.3
TESTSSRC = $(shell ls tests/*.el)
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := compile

.PHONY: test
test: cask compile test-3

.PHONY: test-3
test-3: test-3-tornado test-3-flask

.PHONY: test-3-tornado
test-3-tornado:
	EL_REQUEST_TEST_SERVER=tornado $(MAKE) test-2

.PHONY: test-3-flask
test-3-flask:
	EL_REQUEST_TEST_SERVER=flask $(MAKE) test-2

.PHONY: test-2
test-2: test-2-url-retrieve test-2-curl

.PHONY: test-2-url-retrieve
test-2-url-retrieve:
	EL_REQUEST_BACKEND=url-retrieve $(MAKE) test-1

.PHONY: test-2-curl
test-2-curl:
	EL_REQUEST_BACKEND=curl $(MAKE) test-1

test-1:
	EL_REQUEST_NO_CAPTURE_MESSAGE=$(EL_REQUEST_NO_CAPTURE_MESSAGE) EL_REQUEST_MESSAGE_LEVEL=$(EL_REQUEST_MESSAGE_LEVEL) $(CASK) emacs -Q --batch -L . -L tests -l test-request.el -f ert-run-tests-batch-and-exit

README.rst: README.in.rst request.el
	grep ';;' request.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: compile
compile: cask
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

.PHONY: lint
lint: compile
	bash -ex tools/melpazoid.sh

.PHONY: clean
clean:
	$(CASK) clean-elc
	make -C doc clean
	rm -rf tests/test-install
	rm -rf melpazoid-master/emacs-request

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: compile dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(package-install-file \"dist/request-$(shell $(CASK) version).tar\")"

define SET_GITHUB_ACTOR =
GITHUB_ACTOR := $(shell if [ -z ${GITHUB_ACTOR} ]; then git config user.name; else echo ${GITHUB_ACTOR} ; fi)
endef

define SET_GITHUB_ACTOR_REPOSITORY =
GITHUB_ACTOR_REPOSITORY := $(GITHUB_ACTOR)/$(shell basename `git rev-parse --show-toplevel`)
endef

define SET_GITHUB_HEAD_REF =
GITHUB_HEAD_REF := $(shell if [ -z ${GITHUB_HEAD_REF} ]; then git rev-parse --abbrev-ref HEAD; else echo ${GITHUB_HEAD_REF} ; fi)
endef

define SET_GITHUB_SHA =
GITHUB_SHA := $(shell if [ -z ${GITHUB_SHA} ] ; then git rev-parse origin/${GITHUB_HEAD_REF}; else echo ${GITHUB_SHA}; fi)
endef

define SET_GITHUB_COMMIT =
GITHUB_COMMIT := $(shell if git show -s --format=%s "${GITHUB_SHA}" | egrep -q "^Merge .* into" ; then git show -s --format=%s "${GITHUB_SHA}" | cut -d " " -f2 ; else echo "${GITHUB_SHA}" ; fi)
endef

.PHONY: test-install-vars
test-install-vars:
	$(eval $(call SET_GITHUB_ACTOR))
	$(eval $(call SET_GITHUB_ACTOR_REPOSITORY))
	$(eval $(call SET_GITHUB_HEAD_REF))
	$(eval $(call SET_GITHUB_SHA))
	$(eval $(call SET_GITHUB_COMMIT))
	git show -s --format=%s $(GITHUB_COMMIT)
	# this needs to be the github implicit merge commit
	# e.g., "Merge 8feb544 into 2c33e45",
	git show -s --format=%s $(GITHUB_SHA)

# The timing of the github implicit merge commit for PRs
# is elusive.  So the following won't halt CI.
.PHONY: test-install
test-install: test-install-vars
	mkdir -p tests/test-install
	if [ ! -s "tests/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd tests/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd tests/test-install ; tar xfz $(PKBUILD).tar.gz
	cd tests/test-install ; rm -f $(PKBUILD).tar.gz
	cd tests/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p tests/test-install/recipes
	cd tests/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/request || cp -f ../../../tools/recipe ./request
	! ( $(EMACS) -Q --batch -L tests/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"request\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_ACTOR_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_HEAD_REF)\") \
	               (my-commit \"$(GITHUB_COMMIT)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"request*.el\"))))" 2>&1 | egrep -ia "error: |fatal")
