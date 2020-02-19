export CASK ?= cask
export EMACS ?= $(shell which emacs)
export CASK_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

PKBUILD=2.3
ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),)
TRAVIS_PULL_REQUEST_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(TRAVIS_PULL_REQUEST_SLUG),)
ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),HEAD)
TRAVIS_PULL_REQUEST_SLUG := $(TRAVIS_REPO_SLUG)
else
TRAVIS_PULL_REQUEST_SLUG := $(shell git config --global user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
endif
ifeq ($(TRAVIS),true)
ifeq ($(TRAVIS_PULL_REQUEST_SHA),)
TRAVIS_PULL_REQUEST_SHA := $(shell git rev-parse origin/$(TRAVIS_PULL_REQUEST_BRANCH))
endif
endif

TESTSSRC = $(shell ls tests/*.el)
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := compile

.PHONY: test
test: cask test-3

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
#  global-auto-revert-mode [github #132]
	EL_REQUEST_NO_CAPTURE_MESSAGE=$(EL_REQUEST_NO_CAPTURE_MESSAGE) EL_REQUEST_MESSAGE_LEVEL=$(EL_REQUEST_MESSAGE_LEVEL) $(CASK) emacs -Q --batch -L . -L tests -l test-request.el --eval "(global-auto-revert-mode)" -f ert-run-tests-batch-and-exit

.PHONY: cask
cask: $(CASK_DIR)
$(CASK_DIR): Cask
	$(CASK) install

.PHONY: compile
compile: cask
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

.PHONY: clean
clean:
	$(CASK) clean-elc
	make -C doc clean

.PHONY: test-install
test-install:
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
	--eval "(let* ((my-repo \"$(TRAVIS_PULL_REQUEST_SLUG)\") \
	               (my-branch \"$(TRAVIS_PULL_REQUEST_BRANCH)\") \
	               (my-commit \"$(TRAVIS_PULL_REQUEST_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (concat package-build-archive-dir \"request.el\"))" 2>&1 | egrep -ia "error: |warning: |fatal" )

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
