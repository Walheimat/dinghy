# -- Variables

EMACS?=emacs

DINGHY_DIR?=dinghy
DINGHY_VERSION=0.4.2

DIST_DIR?=dist

LOCAL_DEPS?=$(DIST_DIR)

PACKAGE_SUFFIX?=el
PACKAGE_VERSION=$(shell cask version)

DINGHY_SRC_DIR?=$(DINGHY_DIR)/src
PACIFY=$(DINGHY_SRC_DIR)/dinghy-pacify.el

TEMPLATES_DIR=$(DINGHY_DIR)/templates

UPDATE_VERSION=$(DINGHY_SRC_DIR)/update-version.sh
UPDATE_VERSION_FILES?=Cask $(PACKAGE_NAME).el Makefile
UPDATE_VERSION_CHANGELOG_FILE?=CHANGELOG.md
UPDATE_VERSION_CHANGELOG_HEADING?=Unreleased

CURRENT_PACKAGE_VERSION?="none"
NEXT_RELEASE_VERSION?="none"

# -- Default goal

.PHONY: package-install clean-install ci local

ifdef CI
install: ci
else
install: local
endif

package-install: clean install
	$(EMACS) --batch -f package-initialize \
		--eval "(setq load-prefer-newer t)" \
		--eval "(package-install-file \"$(CURDIR)/$(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION).$(PACKAGE_SUFFIX)\")"

clean-install: clean install

ci: .cask $(CI_DEPS)
	$(info Installed (CI) using dinghy v$(DINGHY_VERSION))

local: $(LOCAL_DEPS) $(LOCAL_PHONY_DEPS)
	$(info Installed using dinghy v$(DINGHY_VERSION))

$(DIST_DIR): .cask
	cask package

.cask: $(CASK_DEPS)
	cask install

# -- Checks

.PHONY: test test-coverage test-tagged test-not-tagged test-selector

TEST_DIR?=test
TEST_HELPER?=$(TEST_DIR)/test-helper.el
TEST_COVERAGE_DIR?=coverage
TEST_EXECUTE_BEFORE=true
TEST_SELECTOR=nil
TEST_TAG?=nil

test: .cask cask-clean
	mkdir -p $(TEST_COVERAGE_DIR)
ifdef TEST_USE_ERT_RUNNER
	$(TEST_EXECUTE_BEFORE) && cask exec ert-runner $(TEST_ARGS)
else
	$(TEST_EXECUTE_BEFORE) && EMACS=$(EMACS) cask emacs --batch -L . -L $(TEST_DIR) \
		-L $(DINGHY_SRC_DIR) \
		--eval '(load-file "$(TEST_HELPER)")' \
		--eval '(dolist (f (nthcdr 2 (directory-files "$(TEST_DIR)" t))) (unless (or (file-directory-p f) (string-suffix-p "$(TEST_HELPER)" f)) (load-file f)))' \
		--eval "(ert-run-tests-batch-and-exit $(TEST_SELECTOR))"
endif

test-coverage: TEST_EXECUTE_BEFORE=export COVERAGE_WITH_JSON=true
test-coverage: test

test-tagged: TEST_SELECTOR=(quote (tag $(TEST_TAG)))
test-tagged: test

test-not-tagged: TEST_SELECTOR=(quote (not (tag $(TEST_TAG))))
test-not-tagged: test

test-selector: TEST_SELECTOR=(symbol-name (quote $(TEST_SELECTOR_STRING)))
test-selector: test

# -- Clean-up

.PHONY: cask-clean clean

cask-clean:
	rm -rf $(DIST_DIR)
	cask clean-elc

clean: cask-clean
	rm -rf $(LOCAL_DEPS)

# -- Utility

.PHONY: update-version update-next-version pacify upgrade-bydi

update-version:
ifneq ($(CURRENT_PACKAGE_VERSION), "none")
	export CHANGELOG_FILE=$(UPDATE_VERSION_CHANGELOG_FILE) && \
		export CHANGELOG_HEADING=$(UPDATE_VERSION_CHANGELOG_HEADING) && \
		$(UPDATE_VERSION) $(CURRENT_PACKAGE_VERSION) $(UPDATE_VERSION_FILES)
else
	$(info You need to set CURRENT_PACKAGE_VERSION in your Makefile)
endif

update-next-version:
ifneq ($(NEXT_PACKAGE_VERSION), "none")
	$(UPDATE_VERSION) $(NEXT_PACKAGE_VERSION) $(UPDATE_VERSION_FILES)
else
	$(info You need to set NEXT_PACKAGE_VERSION as an environment variable)
endif

pacify: $(PACIFY_DEPS)
	EMACS=$(EMACS) cask --batch -L . -l $(PACIFY) $(PACIFY_PRE_EXEC) -f dinghy-pacify-check

CASK_PACKAGE_DIRECTORY=$(shell cask package-directory)

upgrade-bydi:
	$(info Removing bydi from $(CASK_PACKAGE_DIRECTORY))
	cd $(CASK_PACKAGE_DIRECTORY) && rm -rf bydi*
	cask install

# -- Commit linting setup

.PHONY: commits clean-commits
commits: commitlint.config.js .husky

commitlint.config.js:
	$(info Setting up commitlint)
	npm i --save-dev @commitlint/cli
	cp -u $(TEMPLATES_DIR)/commitlint/commitlint.config.js commitlint.config.js

.husky:
	$(info Setting up husky)
	npm i --save-dev husky
	npx husky init
	cp $(TEMPLATES_DIR)/commitlint/commit-msg .husky/commit-msg
	chmod +x .husky/commit-msg
	rm -f .husky/pre-commit

clean-commits:
	$(info Removing commit linting)
	npm remove husky @commitlint/cli
	rm -rf .husky/
	rm -f commitlint.config.js
	git config --unset core.hooksPath

## Semantic-Release

.PHONY: semantic-release

semantic-release:
ifdef PACKAGE_NAME
	$(info Copying semantic-release config, be sure to adjust)
	sed -e 's/%PACKAGE%/$(PACKAGE_NAME)/' $(TEMPLATES_DIR)/semantic-release/.releaserc > .releaserc
else
	$(info You need to set PACKAGE_NAME to copy config)
endif
	$(info Installing dependencies)
	npm install --save-dev semantic-release \
		@semantic-release/changelog \
		@semantic-release/git \
		@semantic-release/exec \
		conventional-changelog-conventionalcommits
	cp -f $(TEMPLATES_DIR)/semantic-release/release.yml .github/workflows/release.yml
	cp -f $(TEMPLATES_DIR)/semantic-release/verify.yml .github/workflows/verify.yml

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:
